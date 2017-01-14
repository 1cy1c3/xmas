Xmas
===============

## Description
Represents a game with retro graphic which a person tries to find all presents with points in a snowy forest. Moreover, wolves try to prevent it.

## Prerequisites

+ GHC
+ GHCI

## Usage
At first, clone or download this project. Afterwards, run your console and compile respectively start the project with the following commands:
```
cd xmas
make
...
./xmas
```
Finally, you are able to play this game. You can move the player with the directional buttons, the button `Q` finishes this game:
```
+---------------------------------------------------+
|               ^                                   |
|     ^          ^                                  |
|                                                   |
|                              ^    ^           ^   |
|  ^   ^                                    ^^  ^   |
|                                   ^               |
|                 ^          ^                      |
|                 ^   ^           ^                 |
|      ^                 ^W          ^          ^   |
|               ^           W^     ^ ^              |
|  ^                                                |
|^                           *              ^       |
|             ^           @         ^         ^^    |
|               ^                      ^  ^     ^^  |
|                                          ^    ^   |
|                      ^           ^   ^            |
|                                       ^         ^ |
|                                                   |
|    ^ ^                                        ^   |
|                           ^                       |
|   ^                  ^                            |
+---------------------------------------------------+
Points: 0   Move: 2   Presents left: 5
```
The symbols stand for:
+ ^: Tree
+ W: Wolf
+ @: Player
+ *: Present
The number of wolves, presents, trees and so on is configured by the file `Config.hs`.

## More information
Read more about Haskell at https://www.haskell.org/documentation. Moreover, there are many comments about this game in the source files.
