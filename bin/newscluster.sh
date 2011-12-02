#!/bin/sh

NEWSCLUSTER_HOME=$HOME/newscluster
SBCL=/usr/local/bin/sbcl

cd "$NEWSCLUSTER_HOME/lib"

case "$1" in
    make-core)
    $SBCL --noinform --disable-debugger --load ./make-core.lisp
    ;;
    *)
    $SBCL --core ./newscluster.core --noinform --disable-debugger  --load command-line.lisp --end-toplevel-options "$@"
    ;;
esac


