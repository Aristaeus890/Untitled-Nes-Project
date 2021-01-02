#!/bin/bash

ca65 game.asm -o game.o -t nes
ld65 game.o -o game.nes -t nes
