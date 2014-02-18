ghc --make -W maze.hs -o maze 
./maze -w 400 -o anim.svg
convert -delay 30 -loop 0 anim*.svg anim.gif
