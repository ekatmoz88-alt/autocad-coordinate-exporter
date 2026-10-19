# AutoCAD text coordinate export

`coord-text-export.lsp` adds the `TXTCOORD` command for exporting selected
single-line `TEXT` entities to a semicolon-separated CSV file.

## Use

1. In AutoCAD, load `coord-text-export.lsp` with `APPLOAD`.
2. Run `TXTCOORD`.
3. Select the single-line text labels to export and press Enter.
4. Choose the coordinate precision (default: 3 decimal places).
5. Choose where to save the CSV file.

The output columns are `X;Y;Text`. Coordinates come from each text entity's
insertion point and are converted to the drawing's WCS. `MTEXT` and other
entity types are not included. Text values are quoted, with embedded double
quotes escaped according to CSV conventions.
