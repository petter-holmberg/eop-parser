# A functional parser for the Elements of Programming language

By Petter Holmberg, 2021

This program implements a (partial) functional parser for the subset of C++
used in the book Elements of Programming (see elementsofprogramming.com).
The language description is based on Appendix B of the book.

The program serves as an example of functionally parsing a non-trivial
grammar in C++. The implementation language is C++20.

The program can interactively parse C++ procedures, structures, enums, and
templates into an abstract syntax tree. If parsing is successful, the
contents of the tree can be printed as formatted source code or as a JSON
object, and the tree can be pruned of redundant nodes.

For simplicity, no expansion of template instantiations is performed if the
parsers encounter a name that would correspond to a prior template definition.
A complete parser for the language would need to handle this case.
Comments are also not parsed. They can be removed in a pre-parsing phase.
