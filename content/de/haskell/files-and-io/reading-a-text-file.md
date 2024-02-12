---
title:                "Textdatei einlesen"
date:                  2024-01-20T17:54:35.492674-07:00
model:                 gpt-4-1106-preview
simple_title:         "Textdatei einlesen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Einlesen einer Textdatei bedeutet, Daten vom Datenträger in den Arbeitsspeicher zu laden, um sie mit einem Programm zu verarbeiten. Programmierer machen das, um Informationen zu nutzen, zu analysieren oder zu manipulieren, die nicht hart in den Quellcode kodiert sind.

## So geht's:
Um eine Textdatei in Haskell zu lesen, nutzen wir die Standardbibliothek. Hier ein kurzes Beispiel:

```Haskell
import System.IO

main :: IO ()
main = do
    fileContent <- readFile "beispiel.txt"
    putStrLn fileContent
```

Ausgabe könnte sein:

```
Hallo, dies ist der Inhalt meiner Datei!
```

Du kannst auch sicherere Methoden mit `withFile` verwenden, um mit dem Datei-Handle direkt zu arbeiten und sicherzustellen, dass die Datei richtig geschlossen wird:

```Haskell
import System.IO

main :: IO ()
main = withFile "beispiel.txt" ReadMode (\handle -> do
    fileContent <- hGetContents handle
    putStrLn fileContent)
```

## Deep Dive
Das Lesen von Dateien in Haskell hat eine reiche Geschichte. Frühe Funktionen wie `readFile` sind für einfache Anwendungen prima, aber sie laden die ganze Datei in den Speicher – problematisch bei sehr großen Dateien. Deshalb führen spätere Versionen von Haskell `ByteString` und `Text`, performante Alternativen für `String`, ein. Mit Streams, etwa durch die Bibliothek `conduit` oder `pipes`, können Daten fließend verarbeitet werden, ohne alles in den Speicher zu laden.

Bezüglich der Implementation, Haskell verwendet Lazy I/O bei Standard-Funktionen wie `readFile`, wodurch die Daten erst gelesen werden, wenn sie im Programm benötigt werden. Das ist magisch, kann aber zu schwer nachverfolgbaren Bugs führen, wenn nicht sorgfältig verwendet. Strikte I/O, zum Beispiel über `Data.Text.IO.readFile`, liest Daten sofort, was oft einfacher zu kontrollieren ist.

## Siehe auch
- [Hackage - Haskell Package Repository](https://hackage.haskell.org/)
- [Learn You a Haskell for Great Good! - Mit Kapiteln über Dateizugriffe](http://learnyouahaskell.com/)
