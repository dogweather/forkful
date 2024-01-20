---
title:                "Eine Textdatei lesen"
html_title:           "Bash: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was und Warum?

Das Lesen einer Textdatei in einer Programmiersprache bedeutet, Daten von der Datei in das Programm zu laden. Programmierer tun dies, um Daten zur weiteren Verarbeitung in ihr Programm aufzunehmen.

## So geht's:

Du kannst in Haskell eine Textdatei lesen, indem du die `readFile` Funktion aus der `Prelude` Bibliothek verwendest.

Haskell-Code sieht folgendermaßen aus:

```Haskell
import System.IO

main = do  
    inhalt <- readFile "test.txt"  
    putStrLn inhalt
```

Wenn die Textdatei "test.txt" die Zeile "Hallo, Haskell!" enthält, sieht die Ausgabe so aus:

```Haskell
Hallo, Haskell!
```

## Tiefgehende Informationen:

Das Konzept des Lesens von Dateien in einer Programmiersprache stammt aus den frühen Tagen der Computerprogrammierung. Es ist eine der primären Methoden zur Interaktion eines Programms mit persistenten Daten.

Es gibt alternative Methoden zum Lesen von Dateien in Haskell, wie die Verwendung von Bibliotheken wie `Data.ByteString` für eine effizientere Verarbeitung von großen Dateien.

Die genaue Implementierung von `readFile` in Haskell ist ein bisschen komplex, da es mit Aspekten wie dem Lazy-loading von Dateiinhalten und der Behandlung von Codierungen umgehen muss.

## Siehe auch:

- [Haskell Documentation: I/O](https://www.haskell.org/tutorial/io.html), offizielle Haskell-Dokumentation zur I/O.
- [Real World Haskell: Input and Output](http://book.realworldhaskell.org/read/io.html), ein hervorragendes Kapitel aus dem Buch "Real World Haskell", das tiefer in die Details von I/O in Haskell eingeht.
- [Learn You a Haskell: Input and Output](http://learnyouahaskell.com/input-and-output), ein weiteres hilfreiches Tutorial zum gleichen Thema.