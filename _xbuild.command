#!/bin/bash
cd /Users/rorden/Documents/pas/Bru2Nii
lazbuild -B ./Bru2Nii.lpr
#lazbuild ./Bru2Nii.lpr --cpu=x86_64 --ws=cocoa --compiler="/usr/local/bin/ppcx64"
strip ./Bru2Nii
lazbuild -B ./Bru2.lpr
strip ./Bru2
mv Bru2 /Users/rorden/Documents/pas/Bru2Nii/compiled/Bru2
mv Bru2Nii /Users/rorden/Documents/pas/Bru2Nii/compiled/Bru2nii.app/Contents/MacOS/Bru2Nii
./_xclean.command


