
# A Cretan Labyrinth generator

This little program generate Cretan Labyrinths. The various parameters allow to generate one of the two possible labyrinth, with various styles (round, square, spiky...). It is also possible to create an animation showing how to draw the labyrinth.
More info in this [blog post](http://www.corentindupont.info/blog/posts/2014-02-17-Cretan-Maze.html).

## Install

    git clone git@github.com:cdupont/diagrams.git
    cd diagrams
    ghc -make 

## Usage

````sh
Usage: maze [-w|--width WIDTH] [-h|--height HEIGHT] [-o|--output OUTPUT] [-l|--loop] [-s|--src ARG] [-i|--interval INTERVAL] [-f|--fpu ARG] [-j|--join <Miter|Round|Bevel>] [-c|--cap <Round|Square|SquareCut>] [-d|--seed <Round|Square|SquareCut>] [-p|--seedPos <0|1>] [-a|--animation]

Available options:
  -?,--help                Show this help text
  -w,--width WIDTH         Desired WIDTH of the output image
  -h,--height HEIGHT       Desired HEIGHT of the output image
  -o,--output OUTPUT       OUTPUT file
  -l,--loop                Run in a self-recompiling loop
  -s,--src ARG             Source file to watch
  -i,--interval INTERVAL   When running in a loop, check for changes every INTERVAL seconds.
  -f,--fpu ARG             Frames per unit time (for animations)
  -j,--join <Miter|Round|Bevel> Style of the joins (choose one)
  -c,--cap <Round|Square|SquareCut> Style of the caps (choose one)
  -d,--seed <Round|Square|SquareCut> Style of the seed (choose one)
  -p,--seedPos <0|1>       Position of the seed (0 or 1)
  -a,--animation           generate an animation
````

The options j, c and d are used to control the style of the joins (how to link two segment of line), caps (how to link a segment of line and a point) and the seed.
The option p is used to choose one of the two possible placement for the seed.

Here are some examples:

    ./maze -w 400 -o maze.svg

![Round style](/doc/maze-round.png "Round style")

    ./maze -w 400 -o maze.svg -p 1

![Round style 2](/doc/maze-round-1.png "Round style, different seed")

    ./maze -w 400 -o maze.svg -d Square -j Miter -c Square -p 1

![Square style](/doc/maze-square.png "Square style")

    ./maze -w 400 -o maze.svg -d SquareCut -j Miter -c SquareCut -p 1

![Spiky](/doc/maze-spiky.png "Spiky")



## Generation animations:

    ./maze -w 400 -o maze.svg -a
    convert -delay 10 -loop 0 maze*.svg maze.gif

Will create an animation:


![Drawing the maze](/doc/maze-anim.gif "Drawing the maze")




