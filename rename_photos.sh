#!/usr/bin/bash

a=1
for i in *; do
  new=$(printf "%04d.jpg" "$a") #04 pad to length of 4
  mv -if -- "$i" "$new"
  let a=a+1
done
for f in [0-9]*; do mv "$f" "`echo $f | sed 's/^[0]*\W*//'`"; done
