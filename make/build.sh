#!/bin/bash


# --- using: LLVM-GCC 4.2 or GCC 4 ---


if [ -z $1 ]
then

   echo
   echo "usage:"
   echo "  build.sh racket|bigloo|chicken|gambit [any other options]"
   echo

else

   # get platform choice
   PLATFORM=$1
   shift

   # choose compiler
   if which llvm-gcc
   then
      COMPILER=llvm-gcc
      LINKER=llvm-gcc
   elif [ -e /Developer/usr/bin/llvm-gcc ]
   then
      COMPILER=/Developer/usr/bin/llvm-gcc
      LINKER=/Developer/usr/bin/llvm-gcc
   else
      COMPILER=gcc
      LINKER=gcc
   fi

   OPT="-O5 -ffast-math -mfpmath=sse -msse"

   # glue all source together into one file
   cat src/platform-$PLATFORM.scm src/general.scm src/objects.scm src/vector3r.scm src/triangle.scm src/surfacepoint.scm src/spatialindex.scm src/scene.scm src/raytracer.scm src/image.scm src/camera.scm src/minilight.scm > minilight-all.scm

   # compile (and link) the file
   if [ $PLATFORM = racket ]
   then
      # Racket 5.3.3

      # bytecode
      #raco make minilight-all.scm
      #mv compiled/minilight-all.scm.zo .
      #rm -r compiled

      # executable
      raco exe -o minilight-scheme-r minilight-all.scm

      # distributable
      #raco distribute minilight-racket minilight-scheme-r

   elif [ $PLATFORM = bigloo ]
   then
      # Bigloo 3.6a
      bigloo $* -cc $COMPILER -copt "$OPT" -O6 -unsafe -stdc -static-bigloo -call/cc -o minilight-scheme-b minilight-all.scm

      rm minilight-all.o

   elif [ $PLATFORM = chicken ]
   then
      # Chicken 4.6.0
      csc $* -cc $COMPILER -ld $LINKER -C "$OPT" -O5 -static-libs -r5rs-syntax -o minilight-scheme-c minilight-all.scm

   elif [ $PLATFORM = gambit ]
   then
      # Gambit-C 4.6.0
      gsc -:s $* -exe -cc-options "$OPT" -o minilight-scheme-g minilight-all.scm

   fi

   rm minilight-all.scm

fi


exit
