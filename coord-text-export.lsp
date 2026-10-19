; Export selected single-line TEXT insertion coordinates to a semicolon CSV.
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

(defun c:TXTCOORD (/ selection precision output-path output-file index entity data insertion wcs-point label count)
  (prompt "\nSelect single-line TEXT entities to export: ")
  (setq selection (ssget '((0 . "TEXT"))))

  (if (null selection)
    (prompt "\nNo TEXT entities selected.")
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
                (setq label (cdr (assoc 1 data)))

                (if (and insertion label)
                  (progn
                    ; TEXT insertion points are stored in the entity's OCS.
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
                  " TEXT label(s) to "
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
