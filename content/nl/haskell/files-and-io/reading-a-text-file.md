---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:53.176244-07:00
description: "Een tekstbestand lezen in programmeren stelt je code in staat gegevens\
  \ te absorberen, zoals het gieten van koffie in je hersenen in de ochtend. We doen\u2026"
lastmod: '2024-03-13T22:44:50.869900-06:00'
model: gpt-4-0125-preview
summary: Een tekstbestand lezen in programmeren stelt je code in staat gegevens te
  absorberen, zoals het gieten van koffie in je hersenen in de ochtend.
title: Een tekstbestand lezen
weight: 22
---

## Hoe:
Hier is hoe je Haskell tekstbestanden laat lezen zonder een druppel zweet te vergieten. Open je favoriete editor, en laten we wat code schrijven.

```Haskell
import System.IO

main = do
    -- Open een bestand in leesmodus
    handle <- openFile "hello.txt" ReadMode
    -- Lees de inhoud van het bestand
    content <- hGetContents handle
    -- Print de inhoud van het bestand
    putStrLn content
    -- Vergeet niet het bestandshandle te sluiten!
    hClose handle
```

Voer dit uit, en als je "hello.txt" hebt met "Hello, World!" erin, krijg je:

```
Hello, World!
```

Hier is een kortere, slimmere manier, hetzelfde doen met minder gedoe:

```Haskell
-- De 'readFile' doet het openen en lezen in één keer
main = do
    content <- readFile "hello.txt"
    putStrLn content
```

De output is nog steeds,

```
Hello, World!
```

## Diepere Duik
Lang geleden waren programma's asociale wezens, die voornamelijk gegevens verwerkten die ze zelf genereerden. Maar complexiteit groeide, en zo ook de behoefte om externe info binnen te trekken, dus werd het lezen van bestanden een basisbehoefte.

Haskell biedt verschillende manieren om bestanden te lezen. We kunnen het op de low-level manier doen met `openFile`, `hGetContents`, en `hClose` of het koeltjes spelen met `readFile`, die alles netjes bundelt.

`readFile` is lui – het leest inhoud wanneer nodig, wat geheugenefficiënt is voor grote bestanden maar kan leiden tot verrassingen als het bestand halverwege verandert. De low-level benadering geeft meer controle, waardoor het voorspelbaarder maar ook uitgebreider is. Voor gigantische teksten helpen Haskell's `hGetLine` of bibliotheken zoals `conduit` en `pipes` het geheugen en de verwerking fijner te beheren.

Haskell's standaard `IO` acties hanteren bestanden met behulp van de onderliggende OS-mechanismen. De bibliotheken abstraheren deze in meer gebruiksvriendelijke operaties, maar aan het einde van de dag, zijn ze gebouwd bovenop Haskell's `IO` monad, wat verzekert dat acties in de juiste volgorde gebeuren.

## Zie Ook
- Voor officiële Haskell-documentatie, bekijk [Haskell's documentatie over invoer en uitvoer](https://www.haskell.org/tutorial/io.html).
- Als je dorst hebt naar meer, savoureer een kopje kennis bij [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/input-and-output).
- Verdiep je begrip van bestandsbeheer met [Real World Haskell's kijk op IO](http://book.realworldhaskell.org/read/io.html).
- Verken streamingbibliotheken voor grote bestanden met [conduit](https://hackage.haskell.org/package/conduit) en [pipes](https://hackage.haskell.org/package/pipes).
