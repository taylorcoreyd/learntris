! Copyright (C) 2016 Corey Taylor.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays ascii columns combinators command-line
fry io kernel locals math math.order math.parser math.ranges
namespaces prettyprint sequences splitting strings system
vectors ;
IN: learntris

<PRIVATE

SYMBOL: game
SYMBOL: score
SYMBOL: cleared
SYMBOL: active-tetr
SYMBOL: continue?
SYMBOL: position
SYMBOL: position-next

TUPLE: matrix grid ;
C: <matrix> matrix

GENERIC: get-grid-at ( y x matrix -- elem )
M: matrix get-grid-at
    grid>> [ swap ] dip nth nth ;

GENERIC: print-grid ( matrix -- )
M: matrix print-grid
    grid>> [ [ " " write ] [ write ] interleave "\n" write ] each flush ;

GENERIC: get-width ( matrix -- num )
M: matrix get-width grid>> first length ;

GENERIC: get-height ( matrix -- num )
M: matrix get-height grid>> length ;

TUPLE: game-grid < matrix ;
C: <game> game-grid

TUPLE: tetramino < matrix start ;
C: <tetramino> tetramino

GENERIC: >uppers ( tetramino -- tetramino )
M: tetramino >uppers dup grid>> [ [ >upper ] map ] map >>grid ;
GENERIC: >lowers ( tetramino -- tetramino )
M: tetramino >lowers dup grid>> [ [ >lower ] map ] map >>grid ;

: transpose ( 2d-Seq -- 2d-Vector )
    dup first [ swap drop ] map-index ! create an array of numbers 0-n
    [ [ dup ] dip <column> >vector ] map >vector ! transpose
    [ drop ] dip ; ! get rid of original

: reverse-rows ( 2d -- 2d ) [ reverse ] map ;

GENERIC: rotate ( tetramino -- tetramino )
M: tetramino rotate
    dup start>> swap ! get a copy of the start position and put it under
    grid>> transpose reverse-rows
    swap <tetramino> ;

: find-first-non-. ( seq -- index/f ) [ "." = not ] find drop ;

: find-first-non-.-from-right ( seq -- index/f ) reverse find-first-non-. ;

GENERIC: get-offset-left ( tetramino -- num )
M: tetramino get-offset-left
    grid>>
    [ find-first-non-. ] map [ f = not ] filter
    10 [ min ] accumulate drop ; ! 10 is bigger than any tetr

! yeah, failed DRY here. But I can't think of a way to factor out the common
! code right now, for little benefit.
GENERIC: get-offset-right ( tetramino -- num )
M: tetramino get-offset-right
    grid>>
    [ find-first-non-.-from-right ] map [ f = not ] filter
    10 [ min ] accumulate drop ;

GENERIC: get-offset-bottom ( tetramino -- num )
M: tetramino get-offset-bottom
    ! if we rotate it to the right, then look for left offset, it's
    ! the same as the bottom offset
    rotate get-offset-left ;

: <shape-i> ( -- tetramino ) V{ V{ "." "." "." "." }
                                V{ "c" "c" "c" "c" }
                                V{ "." "." "." "." }
                                V{ "." "." "." "." } }
    { 0 3 } <tetramino> ;

: <shape-o> ( -- tetramino ) V{ V{ "y" "y" }
                                V{ "y" "y" } }
    { 0 4 } <tetramino> ;

: <shape-z> ( -- tetramino ) V{ V{ "r" "r" "." }
                                V{ "." "r" "r" }
                                V{ "." "." "." } }
    { 0 3 } <tetramino> ;

: <shape-s> ( -- tetramino ) V{  V{ "." "g" "g" }
                                 V{ "g" "g" "." }
                                 V{ "." "." "." } }
    { 0 3 } <tetramino> ;

: <shape-j> ( -- tetramino ) V{ V{ "b" "." "." }
                                V{ "b" "b" "b" }
                                V{ "." "." "." } }
    { 0 3 } <tetramino> ;

: <shape-l> ( -- tetramino ) V{ V{ "." "." "o" }
                                V{ "o" "o" "o" }
                                V{ "." "." "." } }
    { 0 3 } <tetramino> ;

: <shape-t> ( -- tetramino ) V{ V{ "." "m" "." }
                                V{ "m" "m" "m" }
                                V{ "." "." "." } }
    { 0 3 } <tetramino> ;

: <empty-tetr> ( -- tetramino ) V{ V{ "." } } { 0 0 } <tetramino> ;

: empty-row ( -- vector ) V{ } 10 [ "." suffix ] times ;

: init-grid ( -- ) V{ } 22 [ empty-row suffix ] times <game> game set ;

: init ( -- )
    init-grid
    0 score set
    0 cleared set
    t continue? set ;

: use-given-grid ( -- )
    game dup get
    V{ } 22 [ readln suffix ] times ! get the first 22 lines (a game)
    [ " " split >vector ] map
    >>grid set ;

: print-score ( -- ) score get number>string print ;

: increment-score ( -- ) score get 100 + score set ;

: increment-cleared ( -- ) cleared get 1 + cleared set ;

: print-cleared ( -- ) cleared get number>string print ;

: simulate-step ( -- )
    game dup get dup grid>> [
        dup 0 [ "." = [ 1 + ] [ 0 + ] if ] accumulate drop ! count empty
        0 = [ drop empty-row ] [ ] if ! if it's full, replace, count score
    ] map
    >>grid set ;

: move-left ( -- )
    position get second
    active-tetr get get-offset-left +
    0 > [
        position get second
        1 -
        1 position get remove-nth 1 swap insert-nth position set
    ] [ ] if ;

: move-right ( -- )
    position get second
    active-tetr get get-width +
    active-tetr get get-offset-right -
    game get get-width < [
        position get second
        1 +
        1 position get remove-nth 1 swap insert-nth position set
    ] [ ] if ;

: set-active ( tetramino -- )
    dup active-tetr set
    start>> dup position set
    { 1 } swap second suffix position-next set ; ! the next position
    
: print-active ( -- ) active-tetr get print-grid ;

: in-range? ( x a b -- t/f ) ! is x between a and b?
    rot dup -rot ! a x b x
    > -rot <= and ;

: intersect-with-active-at? ( y x position -- t/f )
    dup [ second ! y x @x
    dup active-tetr get grid>> length + ! y x @x @dx
    in-range?
    swap ] dip ! t/f y position
    first
    dup active-tetr get grid>> length +
    in-range? and ;

: intersect-with-active? ( y x -- t/f )
    position get intersect-with-active-at? ;

: get-active-at-game-coords ( y x -- char )
    [ position get first - ] dip
    position get second -
    active-tetr get get-grid-at ;

: tetr-at-empty? ( y x -- t/f )
    get-active-at-game-coords "." = ;

:: get-matrix-with-active ( -- grid )
    game get grid>>
    [ :> y
      [ :> x
        y x intersect-with-active?
        [
            y x tetr-at-empty?
            [ drop y x game get get-grid-at ]
            [ drop y x get-active-at-game-coords ] if
        ]
        [ drop y x game get get-grid-at ] if
      ] map-index
    ] map-index ;

: print-matrix-with-active ( -- )
    active-tetr get >uppers active-tetr set
    get-matrix-with-active <matrix> print-grid ;

: set-down-active-tetr ( -- )
    active-tetr get >lowers active-tetr set
    get-matrix-with-active <game> game set ;

: set-if-needed ( old-y new-y -- set? )
    =
    [ set-down-active-tetr
      <empty-tetr> active-tetr set
      t ]
    [ f ] if ;

: grid-empty-at? ( x y -- t/f ) game get get-grid-at "." = ;
: tetra-empty-at? ( x y -- t/f )
    2dup intersect-with-active? [ tetr-at-empty? ] [ 2drop t ] if ;
: tetra-bottom-collides-at ( y x i -- t/f )
    swap -rot - swap 2dup ! y-i x y-i x
    over game get get-height < [ grid-empty-at? ] [ 2drop f ] if
    [ [ 1 - ] dip tetra-empty-at? ] dip or not ;
: tetra-collides-with-grid-at? ( y x -- t/f )
    2dup 0 tetra-bottom-collides-at ! y is the coordinates right under tetr now
    [ -1 tetra-bottom-collides-at ] dip or ;
: tetra-collides-with-grid? ( y seq-of-xs -- t/f )
    over game get get-height <
    [ [ dupd tetra-collides-with-grid-at? ] map [ ] any? ]
    [ drop t ] if
    [ drop ] dip ;


: next-has-collision? ( -- t/f ) ! yes I know this is way too long.
    ! Actually this is *super* bad.....
    ! Like as bad as it can be. This is embarassing code.
    ! but it works. Will refactor later.
    ! first let's get the y-coordinate we're interested in.
    position get first
    active-tetr get get-height +
    active-tetr get get-offset-bottom -
    ! get a list of x-coords to check
    position get second dup
    active-tetr get get-offset-left + swap
    active-tetr get get-width +
    active-tetr get get-offset-right -
    [a,b) 
    ! y { x1, x2, x3, ... xn }
    tetra-collides-with-grid? ;
    
: move-down ( -- )
    next-has-collision? not ! we are not on bottom
    ! and we have no collisions coming
    [
        position get first
        1 +
        0 position get remove-nth 0 swap insert-nth position set
        position-next get first
        1 +
        0 position-next get remove-nth 0 swap insert-nth position-next set
    ]
    [ ] if ;

: move-drop ( oldpos-y -- )
    move-down
    position get first
    set-if-needed ! gives us a t/f if block is set
    [ ] [ position get first move-drop ] if ;

: get-a-command ( input -- input char )
    dup first 1string dup ! commands char char
    [ dup ?second dup [ 1string ] [ ] if ] 2dip ! 2char 1char 1char
    "?" = ! 2char 1char
    [ swap append [ 1 tail ] dip ]
    [ [ drop ] dip ] if ;

: parse-commands ( commands input -- x )
    get-a-command
    swap [ suffix ] dip ! commands input
    dup length 1 > not [ drop ] [ 1 tail parse-commands ] if ;

: get-commands ( -- commands )
    V{ } readln parse-commands ;

: command ( str/f -- )
    { { f [ f continue? set ] }
      { "q" [ f continue? set ] }
      { "p" [ game get print-grid ] }
      { "P" [ print-matrix-with-active ] }
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
      { ")" [ active-tetr get rotate active-tetr set ] }
      { "(" [ active-tetr get rotate rotate rotate active-tetr set ] }
      { "<" [ move-left ] }
      { ">" [ move-right ] }
      { "v" [ move-down ] }
      { "V" [ position get first move-drop ] }
      [ drop ] } case ;

PRIVATE>

: learntris-main ( -- )
    init
    [ get-commands [ command ] each continue? get ] loop ;

MAIN: learntris-main
