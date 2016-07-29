! Copyright (C) 2016 Corey Taylor.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors combinators command-line io kernel math
math.parser namespaces prettyprint sequences splitting strings
system vectors ;
IN: learntris

<PRIVATE

SYMBOL: score
SYMBOL: cleared
SYMBOL: active-tetr
SYMBOL: continue?

: print-grid ( 2dSeq -- )
    [
        [ " " write ] [ write ] interleave "\n" write
    ] each flush ;

TUPLE: game grid ;
C: <game> game
: print-game ( game -- game )
    dup grid>> print-grid ;


TUPLE: tetramino shape ;
C: <tetramino> tetramino
: print-tetramino ( tetramino -- tetramino )
    dup shape>> print-grid ;
: <shape-i> ( -- tetramino ) V{ V{ "." "." "." "." }
                                V{ "c" "c" "c" "c" }
                                V{ "." "." "." "." }
                                V{ "." "." "." "." } } <tetramino> ;

: <shape-o> ( -- tetramino ) V{ V{ "y" "y" }
                                V{ "y" "y" } } <tetramino> ;

: <shape-z> ( -- tetramino ) V{ V{ "r" "r" "." }
                                V{ "." "r" "r" }
                                V{ "." "." "." } } <tetramino> ;

: <shape-s> ( -- tetramino ) V{  V{ "." "g" "g" }
                                 V{ "g" "g" "." }
                                 V{ "." "." "." } } <tetramino> ;

: <shape-j> ( -- tetramino ) V{ V{ "b" "." "." }
                                V{ "b" "b" "b" }
                                V{ "." "." "." } } <tetramino> ;

: <shape-l> ( -- tetramino ) V{ V{ "." "." "o" }
                                V{ "o" "o" "o" }
                                V{ "." "." "." } } <tetramino> ;

: <shape-t> ( -- tetramino ) V{ V{ "." "m" "." }
                                V{ "m" "m" "m" }
                                V{ "." "." "." } } <tetramino> ;

: empty-row ( -- vector ) V{ } 10 [ "." suffix ] times ;

: init ( -- game )
    ! create a 10x22 game grid
    V{ } 22 [ empty-row suffix ] times
    <game>
    0 score set
    0 cleared set
    t continue? set ;

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

: set-active ( tetramino -- ) active-tetr set ;

: print-active ( -- ) active-tetr get print-tetramino drop ;

: get-commands ( -- x ) readln " " split ;

: command ( game str/f -- game )
    { { f [ f continue? set ] }
      { "q" [ f continue? set ] }
      { "p" [ print-game ] }
      { "g" [ use-given-grid ] }
      { "c" [ clear-grid ] }
      { "?s" [ print-score ] }
      { "?n" [ print-cleared ] }
      { "s" [ simulate-step increment-score increment-cleared ] }
      { "I" [ <shape-i> set-active ] }
      { "O" [ <shape-o> set-active ] }
      { "Z" [ <shape-z> set-active ] }
      { "S" [ <shape-s> set-active ] }
      { "J" [ <shape-j> set-active ] }
      { "L" [ <shape-l> set-active ] }
      { "T" [ <shape-t> set-active ] }
      { "t" [ print-active ] }
      [ drop ] } case ;

PRIVATE>

: learntris-main ( -- )
    init
    [ get-commands [ command ] each continue? get ] loop
    drop ;

MAIN: learntris-main
