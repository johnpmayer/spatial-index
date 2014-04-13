#!/usr/bin/ksh
echo -n "n? "
read n
echo "<Enter> to begin inserting $n items"
read
Main=dist/build/spatial-index/spatial-index
time (echo $n; for i in {1..$n}; do echo $RANDOM; done) | ./$Main

