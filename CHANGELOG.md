# Revision history for brick-skylighting

1.0
===

 * Updated to work with `brick` 1.0.

0.3
===

 * Increased Vty lower bound to 5.23.1 to support italics.
 * Skylighting token styles that used italics now translate to the Vty
   italics style rather than the standout style.

0.2
===

API changes:
 * Added `Brick.Widgets.Skylighting.highlightFromMap` to highlight text by
   looking up syntax names in a Skylighting `SyntaxMap.
 * Removed `Brick.Widgets.Skylighting.simpleHighlight` because we no
   longer have access to the bundled `SyntaxMap` of default entries in
   the `skylighting` package.

Package changes:
 * This now depends on `skylighting-core` (BSD), not `skylighting`
   (GPL), to avoid inadvertent compilation of a GPL depdendency:

   To ensure that users of this library do not inadvertently pull in a
   dependency on a GPL'd library (skylighting), this package uses and
   depends only on the new skylighting-core package, a BSD-compatible
   subset of the skylighting functionality. The main thing we give
   up in doing this is generated Haskell modules that provide syntax
   definitions embedded in the program. Consequently users of this
   library must load the syntax definitions themselves and provide them
   to this library's functions. (skylighting could be used for that
   if desired, but we no longer force users to do so.) Please see the
   README for notes on how to deal with this.

0.1
===

First version.
