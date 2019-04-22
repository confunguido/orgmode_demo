(TeX-add-style-hook
 "manuscript_latex_demo"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art11"
    "inputenc"
    "fontenc"
    "graphicx"
    "grffile"
    "longtable"
    "wrapfig"
    "rotating"
    "ulem"
    "amsmath"
    "textcomp"
    "amssymb"
    "capt-of"
    "hyperref"
    "float"
    "lineno")
   (TeX-add-symbols
    '("lr" 1)
    '("R" 1))
   (LaTeX-add-labels
    "#1"
    "sec:orgc62d1d8"
    "sec:orgd5a8fc2"
    "tbl-costs"
    "eq-ICER"
    "sec:orgd3d5b14"
    "sec:org2e2df3b"
    "fig-epi-benefits"
    "sec:org8a0cfcf"
    "fig-ICER"
    "sec:org3b84392"
    "fig-tornado"
    "table-tornado"
    "sec:org6a13bc5")
   (LaTeX-add-bibliographies
    "Guido_Postdoc_Literature"))
 :latex)

