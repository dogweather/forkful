---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:07.358924-07:00
description: "Controleren of een directory bestaat betekent bevestigen of een map\
  \ daadwerkelijk is waar je denkt dat deze is in het bestandssysteem. Programmeurs\
  \ doen\u2026"
lastmod: '2024-03-11T00:14:24.696732-06:00'
model: gpt-4-0125-preview
summary: "Controleren of een directory bestaat betekent bevestigen of een map daadwerkelijk\
  \ is waar je denkt dat deze is in het bestandssysteem. Programmeurs doen\u2026"
title: Controleren of een directory bestaat
---

{{< edit_this_page >}}

## Wat & Waarom?
Controleren of een directory bestaat betekent bevestigen of een map daadwerkelijk is waar je denkt dat deze is in het bestandssysteem. Programmeurs doen dit om fouten te voorkomen, zoals het proberen te lezen van een niet-bestaande directory of het per ongeluk creëren van dubbele mappen.

## Hoe te:
Haskell gebruikt het `directory`-pakket voor interacties met het bestandssysteem. Installeer het met het commando `cabal install directory` als je dat nog niet hebt gedaan. Zo controleer je een directory:

```Haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
    let dir = "pad/naar/jouw/directory"
    exists <- doesDirectoryExist dir
    putStrLn $ "Bestaat de directory? " ++ show exists
```

Als `dir` bestaat, zal je uitvoer zijn:

```
Bestaat de directory? True
```

Anders zal het tonen:

```
Bestaat de directory? False
```

## Diepere Duik
Vroeger had je misschien direct met systeemaanroepen gewerkt of bibliotheken gebruikt die minder geabstraheerd waren dan `directory`. Nu doet dit Haskell-pakket het zware werk.

Alternatieven? Je zou lagere niveaubewerkingen van het `unix`-pakket kunnen gebruiken, shell-commando's kunnen aanroepen of je eigen FFI-bindings kunnen schrijven. Allemaal overkill voor zo'n eenvoudige controle.

Onder de motorkap gebruikt `doesDirectoryExist` systeemspecifieke aanroepen om de aanwezigheid van de directory te verifiëren zonder een uitzondering te gooien. Het is een IO-actie, vandaar de noodzaak voor de `main`-functie en `IO ()`.

## Zie Ook
Andere bronnen om te overwegen:

- Je lokale Haskell-documentatie: `file:///usr/share/doc/ghc/html/libraries/directory/System-Directory.html`
- Hackage voor het `directory`-pakket: [https://hackage.haskell.org/package/directory](https://hackage.haskell.org/package/directory)
