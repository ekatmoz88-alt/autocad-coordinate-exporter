; Export selected TEXT and MTEXT insertion coordinates to a semicolon CSV.
; Load with APPLOAD, then run the TXTCOORD command.

(defun txtcoord:csv-quote (value / index character result)
  (setq result "\"")
  (setq index 1)
  (while (<= index (strlen value))
    (setq character (substr value index 1))
    (if (= character "\"")
      (setq result (strcat result "\"\""))
      (setq result (strcat result character))
    )
    (setq index (1+ index))
  )
  (strcat result "\"")
)

(defun txtcoord:stack-text (value / index character result)
  (setq index 1)
  (setq result "")
  (while (<= index (strlen value))
    (setq character (substr value index 1))
    (if (or (= character "#") (= character "^"))
      (setq result (strcat result "/"))
      (setq result (strcat result character))
    )
    (setq index (1+ index))
  )
  result
)

(defun txtcoord:mtext-raw (data / item result text-chunk)
  (setq result "")
  (foreach item data
    (if (= (car item) 3)
      (setq result (strcat result (cdr item)))
    )
  )
  (setq text-chunk (cdr (assoc 1 data)))
  (if text-chunk
    (setq result (strcat result text-chunk))
  )
  result
)

(defun txtcoord:mtext-plain (value / index length character next result stacked)
  (setq index 1)
  (setq length (strlen value))
  (setq result "")
  (while (<= index length)
    (setq character (substr value index 1))
    (cond
      ((= character "{")
        (setq index (1+ index))
      )
      ((= character "}")
        (setq index (1+ index))
      )
      ((= character "\\")
        (setq next (if (< index length) (substr value (1+ index) 1) ""))
        (cond
          ((or (= next "P") (= next "X") (= next "~"))
            (setq result (strcat result " "))
            (setq index (+ index 2))
          )
          ((or (= next "\\") (= next "{") (= next "}"))
            (setq result (strcat result next))
            (setq index (+ index 2))
          )
          ((member next '("L" "l" "O" "o" "K" "k"))
            (setq index (+ index 2))
          )
          ((member next '("A" "C" "c" "F" "f" "H" "h" "Q" "q" "T" "t" "W" "w" "p"))
            (setq index (+ index 2))
            (while
              (and
                (<= index length)
                (/= (substr value index 1) ";")
              )
              (setq index (1+ index))
            )
            (if (<= index length)
              (setq index (1+ index))
            )
          )
          ((= next "S")
            (setq index (+ index 2))
            (setq stacked "")
            (while
              (and
                (<= index length)
                (/= (substr value index 1) ";")
              )
              (setq stacked (strcat stacked (substr value index 1)))
              (setq index (1+ index))
            )
            (setq result (strcat result (txtcoord:stack-text stacked)))
            (if (<= index length)
              (setq index (1+ index))
            )
          )
          (T
            (setq result (strcat result next))
            (setq index (+ index 2))
          )
        )
      )
      (T
        (setq result (strcat result character))
        (setq index (1+ index))
      )
    )
  )
  result
)

(defun c:TXTCOORD (/ selection precision output-path output-file index entity data insertion wcs-point label entity-type count)
  (prompt "\nSelect TEXT or MTEXT entities to export: ")
  (setq selection (ssget '((0 . "TEXT,MTEXT"))))

  (if (null selection)
    (prompt "\nNo TEXT or MTEXT entities selected.")
    (progn
      (initget 4)
      (setq precision (getint "\nCoordinate decimal places <3>: "))
      (if (null precision)
        (setq precision 3)
      )

      (setq output-path
        (getfiled
          "Save coordinate catalog"
          (strcat (getvar "DWGPREFIX") "coordinates.csv")
          "csv"
          1
        )
      )

      (if (or (null output-path) (= output-path ""))
        (prompt "\nExport cancelled.")
        (progn
          (setq output-file (open output-path "w"))
          (if (null output-file)
            (prompt "\nCould not open the output file for writing.")
            (progn
              (write-line "X;Y;Text" output-file)
              (setq index 0)
              (setq count 0)

              (while (< index (sslength selection))
                (setq entity (ssname selection index))
                (setq data (entget entity))
                (setq insertion (cdr (assoc 10 data)))
                (setq entity-type (cdr (assoc 0 data)))
                (setq label
                  (if (= entity-type "MTEXT")
                    (txtcoord:mtext-plain (txtcoord:mtext-raw data))
                    (cdr (assoc 1 data))
                  )
                )

                (if (and insertion label)
                  (progn
                    ; TEXT and MTEXT insertion points are stored in the entity's OCS.
                    ; Convert to WCS so exported X and Y use drawing coordinates.
                    (setq wcs-point (trans insertion entity 0))
                    (write-line
                      (strcat
                        (rtos (car wcs-point) 2 precision)
                        ";"
                        (rtos (cadr wcs-point) 2 precision)
                        ";"
                        (txtcoord:csv-quote label)
                      )
                      output-file
                    )
                    (setq count (1+ count))
                  )
                )

                (setq index (1+ index))
              )

              (close output-file)
              (prompt
                (strcat
                  "\nExported "
                  (itoa count)
                  " label(s) to "
                  output-path
                )
              )
            )
          )
        )
      )
    )
  )
  (princ)
)
