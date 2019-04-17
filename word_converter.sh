#!/bin/bash
pandoc manuscript_orgmode_demo.tex -o manuscript_orgmode_demo.docx --mathml --filter pandoc-citeproc --csl plos-computational-biology.csl
