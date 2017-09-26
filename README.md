flatcurry
=========

Libraries to deal with FlatCurry programs.

Currently, it contains the following libraries:

* `FlatCurry.Compact`: This module contains operations to reduce the size
  of FlatCurry programs by combining the main module and all imports
  into a single program that contains only the functions directly or
  indirectly called from a set of main functions.
* `FlatCurry.XML`: This module contains operations to convert FlatCurry
  programs into corresponding XML expressions and vice versa.
  This can be used to store Curry programs in a way independent
  of a Curry system or to use a Curry system, like PAKCS,
  as back end by other functional logic programming systems.
