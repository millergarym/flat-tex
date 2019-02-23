flat-tex
========

The use case for this tool is:
your paper has been accepted at a conference, and now you have
to submit the latex sources for inclusion in the proceedings.

But - your sources contain some information that you don't want
to reveal. E.g., it contains  `\todo{fix proof of previous theorem}`.

`flat-tex` does the following:
* flatten a multi-file latex document (recursively expanding all `\input` and `\bibliography`)
* remove all `\todo{..}`, `\reminder{..}` and comments (`% ...`)

How `\input{foo}` is expanded:
* if the file `foo` does exist, then it is processed
* else `foo.tex` is processed

How `\bibliography{...}`is expanded:
* the file `top.bbl` is processed, where `top.tex` is the file that contains the `\bibliography`  statement

