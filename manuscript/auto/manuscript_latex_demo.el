(TeX-add-style-hook
 "manuscript_latex_demo"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
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
    "sec:orgcd02ace"
    "sec:orgec99251"
    "tbl-costs"
    "eq-ICER"
    "sec:org73cde39"
    "sec:org3968e12"
    "fig-epi-benefits"
    "sec:org1753f5e"
    "fig-ICER"
    "sec:orgf1237fe"
    "fig-tornado"
    "table-tornado"
    "sec:org4812a88")
   (LaTeX-add-bibliographies
    "Guido_Postdoc_Literature"))
 :latex)

