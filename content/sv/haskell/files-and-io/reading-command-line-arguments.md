---
date: 2024-01-20 17:56:25.864519-07:00
description: "Att l\xE4sa kommandoradsargument \xE4r att f\xE5nga upp de textbitar\
  \ du anger n\xE4r du k\xF6r ett program i terminalen. Programmers g\xF6r det f\xF6\
  r att l\xE5ta anv\xE4ndare\u2026"
lastmod: '2024-03-13T22:44:37.970234-06:00'
model: gpt-4-1106-preview
summary: "Att l\xE4sa kommandoradsargument \xE4r att f\xE5nga upp de textbitar du\
  \ anger n\xE4r du k\xF6r ett program i terminalen."
title: "L\xE4sa in kommandoradsargument"
weight: 23
---

## Hur gör man:
I Haskell, använd modulen `System.Environment` som ger dig funktioner för att hantera kommandoradsargument. Här är ett enkelt exempel:

```Haskell
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    putStrLn ("Hej! Du gav mig följande argument: " ++ unwords args)
```

Om du sparar detta i `HejArgs.hs` och kör det så här:

```
runghc HejArgs.hs glad att träffa dig
```

Får du output:

```
Hej! Du gav mig följande argument: glad att träffa dig
```

## Djupdykning
Historiskt har kommandoradsargument varit ett sätt att interagera med många tidiga program och script. I Haskell kan du också använda paket som `optparse-applicative` för att skapa mer avancerade argumentparsrar med hjälp av monadiska mönster, vilket ger en kraftfull, komponerbar gränssnitt för kommandotolkbaserade applikationer. Implementationen av kommandoradsargument i Haskell är rakt på sak: `getArgs` hämtar en lista med `String` från det underliggande systemskalet, medan `getProgName` kan användas för att få programnamnet som används.

## Se också
- [Haskell Documentation for System.Environment](https://hackage.haskell.org/package/base/docs/System-Environment.html)
- [optparse-applicative on Hackage](https://hackage.haskell.org/package/optparse-applicative)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/chapters) - En bra resurs för nya Haskell-programmerare.
- [Real World Haskell](http://book.realworldhaskell.org/) - Mer djupgående information om att bygga praktiska Haskell-applikationer.
