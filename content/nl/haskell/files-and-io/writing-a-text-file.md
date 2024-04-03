---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:53.692433-07:00
description: "In Haskell is het schrijven naar een tekstbestand alles over het opslaan\
  \ van data in een bestand. Programmeurs doen dit om data tussen sessies te bewaren,\u2026"
lastmod: '2024-03-13T22:44:50.870886-06:00'
model: gpt-4-0125-preview
summary: In Haskell is het schrijven naar een tekstbestand alles over het opslaan
  van data in een bestand.
title: Een tekstbestand schrijven
weight: 24
---

## Hoe:
Tekstbestanden schrijven in Haskell is vrij eenvoudig. Hier is de kern met het gebruik van `writeFile`:

```Haskell
import System.IO

main :: IO ()
main = do
  let content = "Hallo, bestand!"
  writeFile "groeten.txt" content
```

Deze code genereert een bestand `groeten.txt` met "Hallo, bestand!" erin.

Voor het toevoegen van tekst, gebruik `appendFile`:

```Haskell
appendToFile :: FilePath -> String -> IO ()
appendToFile file content = appendFile file content

-- Gebruik
main :: IO ()
main = appendToFile "groeten.txt" "\nTot ziens!"
```

Nu zal `groeten.txt` ook "Tot ziens!" aan het einde hebben.

## Diepere Duik
De functies voor het schrijven naar bestanden in Haskell komen voort uit de robuuste IO-afhandeling. `writeFile` en `appendFile` zijn handige wrappers rondom lagere-niveau operaties. Alternatieven zoals `hPutStr` of `hPutStrLn` bieden meer controle, waarmee we een bestandshandle kunnen specificeren.

Details:
- `writeFile`: maakt het bestand leeg voor het schrijven.
- `appendFile`: maakt niet leeg, voegt alleen toe aan het einde.
- Beide gaan om met tekstcodering en buffering.
- Voor niet-tekst data, gebruik functies zoals `hPutBuf`.

## Zie Ook
Voor meer informatie en beste praktijken:

- [Haskell Documentatie over IO](https://haskell.org/documentation)
- [Learn You a Haskell for Great Good! - IO](http://learnyouahaskell.com/input-and-output)
- [Real World Haskell - Werken met Bestanden](http://book.realworldhaskell.org/read/io.html)
