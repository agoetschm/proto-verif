# ProVerif pet implementation in Scala

**Note**: this project is not completely finished. 
The derivation results might be wrong, the output would first need to be made clearer to enable further developement/debugging. 
See my [blog post](https://ango.dev/blog/proto-verif/) (or [here](./README_blog.adoc)) for an explanation and a guide through the code structure.

## Getting started

The best way to see what is happening is to run the tests.
[`sbt`](https://www.scala-sbt.org/) lets you run some specific test file only, for example the one derivating the man-in-the-middle attack on the Needham Schroeder protocol.

`sbt test`

`sbt 'testOnly *NeedhamSchroeder'`

`sbt 'testOnly *NeedhamSchroederLowe'`
