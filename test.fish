#!/opt/homebrew/bin/fish

set filename $argv[1]

cat $filename | ./codegen

delta (cat iloc.out | psub) (cat solutions/(basename $filename).iloc | psub)
