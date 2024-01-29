---
title:                "Schrijven naar standaardfout"
date:                  2024-01-28T22:13:40.191547-07:00
model:                 gpt-4-0125-preview
simple_title:         "Schrijven naar standaardfout"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/haskell/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Schrijven naar standaardfout (stderr) in Haskell laat je fouten en debug informatie rapporteren, los van de standaarduitvoer (stdout). Dit wordt gedaan om de uitvoerstromen georganiseerd te houden, waardoor het gemakkelijker wordt om alleen om te gaan met wat nodig is—zoals het doorvoeren van uitvoer of het loggen van fouten.

## Hoe:

Gebruik System.IO om naar stderr te schrijven. Hier is een eenvoudige demo:

```Haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "Dit gaat naar stderr"
  putStrLn "Dit gaat naar stdout"
```

Uitvoer bij het uitvoeren van het programma:

```
Dit gaat naar stdout
```

Om de stderr-uitvoer te zien, redirect je deze:

```bash
runhaskell jouw_programma.hs 2> fout.log
```

`fout.log` bevat nu "Dit gaat naar stderr".

## Diepere Duik

Het IO-systeem van Haskell onderscheidt tussen stdout en stderr, waarbij de Unix-conventies in stand worden gehouden. Voor Haskell stelden talen zoals C het precedent van gescheiden stromen in—stdout voor resultaten, stderr voor fouten en logs.

Alternatieve manieren van uitvoer omvatten het gebruiken van bibliotheken zoals `System.Log.Logger` voor meer complexe logboekregistratie. Met betrekking tot implementatie is stderr in Haskell een `Handle`, net als een bestandshandle, maar vooraf gedefinieerd om naar de systeemfoutuitvoer te verwijzen.

## Zie Ook

- [Haskell System.IO-bibliotheek](https://hackage.haskell.org/package/base-4.16.0.0/docs/System-IO.html): Gedetailleerde documentatie over System.IO.
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/): Een inleidend boek over Haskell dat I/O behandelt.
