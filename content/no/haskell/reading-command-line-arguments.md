---
title:                "Lese kommandolinjeargumenter"
date:                  2024-01-20T17:56:15.954553-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Lese kommandolinjeargumenter betyr å hente inn data som brukeren gir når de kjører programmet ditt. Det gjør vi for å gi fleksibilitet og tilpasse oppførselen til programmet uten å hardkode verdier.

## Hvordan:
For å lese argumenter fra kommandolinjen i Haskell, kan du bruke `getArgs` fra `System.Environment`. Her er et enkelt eksempel:

```Haskell
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    print args
```

Kjører du programmet med `runghc myprogram.hs arg1 arg2 arg3`, blir resultatet:

```
["arg1", "arg2", "arg3"]
```

## Dykk Dypere:
I Haskell, som i mange andre språk, er kommandolinjeargumenter essensielle for interaktivitet. Det startet med UNIX og C, hvor `int main(int argc, char *argv[])` lar deg håndtere argumenter.

Det finnes alternativer til `getArgs`, som `getProgName` for programnavnet, eller mer avanserte biblioteker som `optparse-applicative` for rike og komplekse argumenter. Selve implementasjonen i Haskell håndterer argumentene som en liste av strenger, noe som gjør det lett å jobbe med.

Det er også viktig å validere og håndtere forventede og uventede argumenter på en sikker måte for å unngå feil i kjøringen av programmet.

## Se Også:
- [Haskell System.Environment Documentation](https://hackage.haskell.org/package/base-4.16.0.0/docs/System-Environment.html)
- [optparse-applicative på Hackage](https://hackage.haskell.org/package/optparse-applicative) for kompleks argumenthåndtering
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/) for en mer omfattende introduksjon til Haskell