---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:09.714148-07:00
description: 'Hoe: Haskell biedt het `temporary` pakket, dat handige functies bevat
  voor tijdelijke bestandsoperaties. Hier is een snelle demonstratie.'
lastmod: '2024-03-13T22:44:50.871837-06:00'
model: gpt-4-0125-preview
summary: Haskell biedt het `temporary` pakket, dat handige functies bevat voor tijdelijke
  bestandsoperaties.
title: Een tijdelijk bestand aanmaken
weight: 21
---

## Hoe:
Haskell biedt het `temporary` pakket, dat handige functies bevat voor tijdelijke bestandsoperaties. Hier is een snelle demonstratie:

```haskell
import System.IO.Temp (withSystemTempFile)
import System.IO (hPutStrLn, hClose)

main :: IO ()
main = withSystemTempFile "mytemp.txt" $ \tempFilePath tempFileHandle -> do
    -- Schrijf iets naar het tijdelijke bestand
    hPutStrLn tempFileHandle "Hallo, tijdelijk bestand!"
    -- Sluit het bestand (gebeurt ook automatisch!)
    hClose tempFileHandle
    putStrLn $ "Er is een tijdelijk bestand gemaakt op: " ++ tempFilePath
```

Voorbeelduitvoer:

```
Er is een tijdelijk bestand gemaakt op: /tmp/mytemp.txt123456
```

## Diepere Duik
Vroeger kon het beheren van tijdelijke bestanden lastig zijn en riskant voor racecondities — twee programma's die proberen hetzelfde bestand te maken of te gebruiken. Daarom is Haskell’s `temporary` pakket gemaakt. Het geeft je functies zoals `withSystemTempFile`, die een tijdelijk bestand creëert en het automatisch opruimt als je klaar bent. Heel netjes om je bestandsoperaties strak en opgeruimd te houden.

Er zijn alternatieven zoals het gebruik van het `unix` pakket voor de nitty-gritty bestandsoperaties, maar `temporary` abstraheert de complexiteit weg. Bij het gebruik van `temporary` zijn bestandsnamen uniek dankzij interne functies. Geen twee tijdelijke bestanden zullen botsen, wat je leven een beetje makkelijker maakt.

De magie in de aanpak van Haskell omvat zijn functionele aard, zorgend dat bijeffecten, zoals bestandscreatie, zorgvuldig worden afgehandeld. Het leunt op zijn typesysteem en IO monad om verantwoordelijk met bronnen om te gaan.

## Zie Ook
- [`System.IO.Temp` documentatie](https://hackage.haskell.org/package/temporary-1.3/docs/System-IO-Temp.html): Officiële documentatie voor de tijdelijke bestandsfuncties.
- [Real-World Haskell, Hoofdstuk 7, I/O](http://book.realworldhaskell.org/read/io.html): Een boeksectie die Haskell I/O uitlegt, en in meer detail de creatie van tijdelijke bestanden behandelt.
