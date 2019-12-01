#!/bin/bash

DIR=day$(printf "%02d" $1)

create ()
{
cat > $1 <<- "EOF"
#!/usr/bin/env stack
-- stack script --resolver lts-14.16

main :: IO ()
main = putStrLn "Advent Of Code"
EOF
}

mkdir -p $DIR
create $DIR/first.hs
chmod +x $DIR/first.hs
create $DIR/second.hs
chmod +x $DIR/second.hs
