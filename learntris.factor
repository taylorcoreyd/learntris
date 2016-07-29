! Copyright (C) 2016 Corey Taylor.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors columns combinators command-line io kernel math
math.parser namespaces prettyprint sequences splitting strings
system vectors ;
IN: learntris

<PRIVATE

SYMBOL: game
SYMBOL: score
SYMBOL: cleared
SYMBOL: active-tetr
SYMBOL: continue?

: print-grid ( 2dSeq -- )
    [
        [ " " write ] [ write ] interleave "\n" write
    ] each flush ;

TUPLE: game-grid grid ;
C: <game> game-grid
: print-game ( -- )
    game get grid>> print-grid ;


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

! we can rotate by first transposing, then reversing each row.
: rotate ( -- )
    active-tetr get shape>>
    dup first [ swap drop ] map-index ! create an array of numbers 0-n
    [ [ dup ] dip <column> >vector ] map >vector ! transpose
    [ drop ] dip ! get rid of original
    V{ } swap [ reverse suffix ] each
    <tetramino> active-tetr set ;

: empty-row ( -- vector ) V{ } 10 [ "." suffix ] times ;

: init-grid ( -- ) V{ } 22 [ empty-row suffix ] times <game> game set ;

: init ( -- )
    init-grid
    0 score set
    0 cleared set
    t continue? set ;

: use-given-grid ( -- )
    V{ } 22 [ readln suffix ] times ! get the first 22 lines (a game)
    [ " " split >vector ] map
    <game> game set ;

: print-score ( -- ) score get number>string print ;

: increment-score ( -- ) score get 100 + score set ;

: increment-cleared ( -- ) cleared get 1 + cleared set ;

: print-cleared ( -- ) cleared get number>string print ;

: simulate-step ( -- )
    game get grid>> [
        dup 0 [ "." = [ 1 + ] [ 0 + ] if ] accumulate drop ! count empty
        0 = [ drop empty-row ] [ ] if ! if it's full, replace, count score
    ] map <game> game set ;

: set-active ( tetramino -- ) active-tetr set ;

: print-active ( -- ) active-tetr get print-tetramino drop ;

: print-matrix-with-active ( -- ) ;
    
: get-commands ( -- x ) readln " " split ;

: command ( str/f -- )
    { { f [ f continue? set ] }
      { "q" [ f continue? set ] }
      { "p" [ print-game ] }
      { "g" [ use-given-grid ] }
      { "c" [ init-grid ] }
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
      { ";" [ nl flush ] }
      { ")" [ rotate ] }
      [ drop ] } case ;

PRIVATE>

: learntris-main ( -- )
    init
    [ get-commands [ command ] each continue? get ] loop ;

MAIN: learntris-main
