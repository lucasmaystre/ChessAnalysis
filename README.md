# TrueSkill Through Time

This is an attempt to build and use the TrueSkill Through Time codebase on a
modern Linux system.

The original code was released by Microsoft Research Cambridge.

- [2008 blog post][1]
- [2012 blog post][2]


## Quickstart

On a recent version of Ubuntu (e.g., 16.04), do

    sudo apt-get install fsharp
    xbuild /p:Configuration=Release ChessAnalysis.fsproj
    mono bin/Release/ChessAnalysis.exe -out output.csv SmallChessBase.csv

*et voilà!*

[1]: https://blogs.technet.microsoft.com/apg/2008/04/05/trueskill-through-time/
[2]: https://blogs.msdn.microsoft.com/dsyme/2012/04/19/updated-version-of-trueskill-through-time-bayesian-inference-code/
