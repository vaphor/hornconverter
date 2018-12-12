# Hornconverter
# from mini java to horn clauses

Julien Braine, Laure Gonnord, David Monniaux, 2016-2018
Licence: GPL

Input: a source code conform to "mini-java" syntax
Output: a smt2 file conform to the smtlib syntax 
the output still contains usage of the array theory.

Usage : ./java2horn [options] file

Example : ./java2horn demo.java -o demo.smt2
