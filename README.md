# Hornconverter
# from mini java to horn clauses

Julien Braine, Laure Gonnord, David Monniaux, 2016-2018
Licence: GPL

Input: a source code conform to "mini-java" syntax
Output: a smt2 file conform to the smtlib syntax 
the output still contains usage of the array theory.

Usage : ./converter [options] file

Example : ./converter demo.java -o demo.smt2

Use your favorite SMT solver to solve the smt2 file
or use another vaphor tool to abstract it into a smt2 without arrays.
