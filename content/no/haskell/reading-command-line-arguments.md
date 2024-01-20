---
title:                "Lese kommandolinjeargumenter"
html_title:           "Arduino: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Lesing av kommandolinjeargumenter i Haskell: en enkel guide

## Hva og hvorfor?
Kommandolinjeargumenter er parametere som følger etter programnavnet når det kjøres i terminalen. Det å kunne lese disse argumentene gir fleksibilitet ved å tillate brukerinput utenfor selve programmet.

## Slik gjør du det:
Haskell gir en innebygd funksjon kalt `getArgs` for å hente kommandolinjeargumentene. Her er en enkel bit kode som viser hvordan man gjør det.

```Haskell
import System.Environment
main :: IO ()
main = do
    args <- getArgs
    print args
```

Når du kjører dette programmet og skriver argumenter etter programnavnet, vil det trykke argumentene til konsollen. For eksempel:

```
$ ghc Args.hs
$ ./Args Hei Haskell
["Hei", "Haskell"]
```

## Dypdykk
Historisk sett har kommandolinjeargumenter vært en måte å parametrisere programmer på siden tidlig utvikling av Unix-systemer. I Haskell, kan du benytte funksjonen `getArgs` som vi viste tidligere, men det finnes også biblioteker som `optparse-applicative` som gir en mer kraftfull og fleksibel håndtering av argumenter.

En interessant ting å merke seg er at `getArgs` returnerer en liste over `String`. Dette er fordi den tolker argumentene som separate strenge hvis de er adskilt av mellomrom på kommandolinjen.

## Se også
Hvis du vil læle mer om dette emnet, er her noen nyttige kilder:
- [Biblioteksdokumentasjon for System.Environment](https://hackage.haskell.org/package/base-4.12.0.0/docs/System-Environment.html)
- [Dokumentasjon for optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)