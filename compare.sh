#!/bin/zsh

REFC="scala -cp ./reference/koolc_2.11-1.3.1.jar:./reference/cafebabe_2.11-1.2.jar koolc.Main"
MYC="scala -cp ./target/scala-2.11/koolc_2.11-1.0.jar:./lib/cafebabe_2.10-1.2.jar koolc.Main"
TESTDIR="./testprograms/lab3/valid"

REFD=$(mktemp -d --tmpdir "kool.ref.XXX")
OWND=$(mktemp -d --tmpdir "kool.own.XXX")
REFO=$(mktemp --tmpdir "kool.ref.out.XXX")
OWNO=$(mktemp --tmpdir "kool.own.out.XXX")

CLASS=$(basename $1 ".kool")
[[ -n "$2" ]] && CLASS=$2
${=REFC} $TESTDIR/$1 -d $REFD
${=MYC} $TESTDIR/$1 -d $OWND
java -cp $REFD $CLASS > $REFO
java -cp $OWND $CLASS > $OWNO
sdiff $REFO $OWNO
diff -q $REFO $OWNO

rm -r $REFD $OWND $REFO $OWNO
