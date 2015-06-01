#!/bin/bash
cd /Users/rorden/Documents/pas/Bru2Nii
lazbuild -B ./Bru2Nii.lpr
strip ./Bru2Nii
lazbuild -B ./Bru2.lpr
strip ./Bru2
mv Bru2 /Users/rorden/Documents/pas/Bru2Nii/compiled/Bru2
mv Bru2Nii /Users/rorden/Documents/pas/Bru2Nii/compiled/Bru2nii.app/Contents/MacOS/Bru2Nii



