---
title:                "Commandoregelargumenten lezen"
aliases:
- /nl/haskell/reading-command-line-arguments.md
date:                  2024-01-28T22:05:10.039512-07:00
model:                 gpt-4-0125-preview
simple_title:         "Commandoregelargumenten lezen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/haskell/reading-command-line-arguments.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het lezen van commandoregelargumenten in Haskell laat je gebruikersinvoer opsnuiven wanneer ze je programma uitvoeren. Waarom? Om het gedrag van het programma on the fly aan te passen, zonder de code zelf te wijzigen.

## Hoe te:

```haskell
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("Hallo, " ++ show args ++ "!")
```

Voer het uit door "wereld" als een argument door te geven:

```bash
$ runhaskell jouwprogramma.hs wereld
Hallo, ["wereld"]!
```

## Diepgaande Duik

Haskell is een nette taal, met wortels in de jaren 80, die zuiverheid en statische typen vooropstelt. Al vanaf de vroege dagen heeft het een manier om met commandoregelargumenten om te gaan. In andere talen kan dit vrij procedureel spul zijn, maar hier bevinden we ons in het rijk van IO monaden om de wilde buitenwereld aan te pakken.

Alternatieven? Je kunt je helemaal uitleven met bibliotheken zoals `optparse-applicative` voor complexe zaken, maar voor eenvoudige gevallen doet `getArgs` de truc.

Achter de schermen? `getArgs` is een functie die in je systeem duikt, ophaalt wat er op de programmanaam in de terminal volgde, en je een lijst met strings geeft. Het is geïmplementeerd in de basisbibliotheek van Haskell, leunend op lagere niveau C-functies om het zware werk te doen. Netjes, toch?

## Zie Ook

- Dieper ingaan op `getArgs`: [Hoogle op System.Environment](https://hoogle.haskell.org/?hoogle=System.Environment.getArgs)
- Niveau omhoog in argumenten parsen: [optparse-applicative op Hackage](https://hackage.haskell.org/package/optparse-applicative)
