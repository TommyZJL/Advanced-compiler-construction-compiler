                           _________________

                            THE L₃ COMPILER
                           _________________





1 Introduction
==============

  This directory contains the source code of the L₃ compiler, written in
  Scala. All interactions with the compiler should be done with [`sbt'],
  a Scala build tool.

  `Sbt' can either be run in interactive mode, by simply typing `sbt'
  and then entering commands at the prompt, or in batch mode. The
  following sections use batch mode for illustration, but in practice
  interactive mode is often to be preferred as it avoids repeated
  startup of `sbt' itself.


  [`sbt'] http://www.scala-sbt.org/


2 Compiling
===========

  To compile the compiler, use the `compile' command:
  ,----
  | $ sbt compile
  `----
  (the dollar sign `$' represents the shell prompt).


3 Testing
=========

  To test the compiler (and compile it beforehand, if necessary), use
  the `test' command:
  ,----
  | $ sbt test
  `----


4 Running
=========

  To run the compiler (and compile it beforehand, if necessary), use the
  `run' command, followed by arguments for the compiler, e.g.:
  ,----
  | $ sbt run ../library/lib.ml3 ../examples/queens.l3
  `----

  The compiler accepts a list of files to compile as arguments. These
  files can have one of the following extensions:

  `.l3': A normal source file, containing L₃ code.

  `.ml3': A module file, containing a list of other files, which must also
          be either source files (with a `.l3' extension) or other module
          files (with a `.ml3' extension).

  Modules are expanded recursively, until only `.l3' files remain. Then,
  duplicate file names are removed, with only the first occurrence kept.
  Finally, this list of files is fed to the compiler.

  As an example, assume that the file `lib.ml3' references
  `characters.ml3' and `integers.ml3', and that `characters.ml3'
  references `characters.l3' while `integers.ml3' references both
  `characters.ml3' and `integers.l3'. Then, a command line consisting of
  `lib.ml3' and `helloworld.l3' is expanded as follows:

  1. `lib.ml3' `helloworld.l3' (original command line),

  2. `characters.ml3' `integers.ml3' `helloworld.l3' (expansion of
     `lib.ml3'),

  3. `characters.l3' `characters.ml3' `integers.l3' `helloworld.l3'
     (expansion of `characters.ml3' and `integers.ml3'),

  4. `characters.l3' `characters.l3' `integers.l3' `helloworld.l3'
     (expansion of the second `characters.ml3'),

  5. `characters.l3' `integers.l3' `helloworld.l3' (removal of
     duplicates).
