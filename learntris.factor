! Copyright (C) 2016 Corey Taylor.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors combinators command-line io kernel math
math.parser namespaces prettyprint sequences splitting strings
system vectors ;
IN: learntris

<PRIVATE

SYMBOL: score
SYMBOL: cleared

TUPLE: game grid ;
C: <game> game
: print-game ( game -- game )
    dup grid>> [
        [ swap write 1 + 10 < [ " " write ] [ "\n" write ] if ] each-index
    ] each flush ;

: empty-row ( -- vector ) V{ } 10 [ "." suffix ] times ;

: init ( -- game )
    ! create a 10x22 game grid
    V{ } 22 [ empty-row suffix ] times
    <game>
    0 score set
    0 cleared set ;

: use-given-grid ( game -- game )
    drop ! the init game is useless to us now.
    V{ } 22 [ readln suffix ] times ! get the first 22 lines (a game)
    [ " " split >vector ] map
    <game> ;

: clear-grid ( game -- game )
    drop init ;

: print-score ( -- ) score get number>string print ;

: increment-score ( -- ) score get 100 + score set ;

: increment-cleared ( -- ) cleared get 1 + cleared set ;

: print-cleared ( -- ) cleared get number>string print ;

: simulate-step ( game -- game )
    grid>> [
        dup 0 [ "." = [ 1 + ] [ 0 + ] if ] accumulate drop ! count empty
        0 = [ drop empty-row ] [ ] if ! if it's full, replace, count score
    ] map <game> ;

: command ( game str/f -- game continue? )
    { { f [ f ] }
      { "q" [ f ] }
      { "p" [ print-game t ] }
      { "g" [ use-given-grid t ] }
      { "c" [ clear-grid t ] }
      { "?s" [ print-score t ] }
      { "?n" [ print-cleared t ] }
      { "s" [ simulate-step increment-score increment-cleared t ] }
      [ drop t ] } case ;

PRIVATE>

: learntris-main ( -- )
    init
    [ readln command ] loop
    drop ;

MAIN: learntris-main
