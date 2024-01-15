---
title:                "Ein Textdokument lesen."
html_title:           "Haskell: Ein Textdokument lesen."
simple_title:         "Ein Textdokument lesen."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Warum sollten wir uns mit dem Lesen von Textdateien beschäftigen? Nun, zum einen ist es eine Grundfertigkeit, die in der Programmierung immer wieder benötigt wird. Zum anderen kann es sehr nützlich sein, um Daten zu importieren oder zu exportieren und somit mit anderen Anwendungen zu interagieren.

## Wie geht's

Um eine Textdatei in Haskell zu lesen, müssen wir zuerst die Module `System.IO` und `Data.Char` importieren.

```Haskell
import System.IO
import Data.Char
```

Dann müssen wir eine Datei öffnen, um sie lesen zu können. Dazu verwenden wir die Funktion `openFile`, die den Dateipfad und den Modus, in dem die Datei geöffnet werden soll, als Argumente nimmt. Hier öffnen wir die Datei "beispiel.txt" im Lese-Modus und speichern sie in der Variablen `handle`.

```Haskell
handle <- openFile "beispiel.txt" ReadMode
```

Als nächstes müssen wir den Inhalt der Datei lesen. Dazu nutzen wir die Funktion `hGetContents`, die den geöffneten Datei-Handle als Argument nimmt und uns den gesamten Inhalt der Datei als String zurückgibt.

```Haskell
contents <- hGetContents handle
```

Der zurückgegebene String enthält nun den gesamten Inhalt der Datei, einschließlich aller Zeilenumbrüche und Leerzeichen. Wenn wir nur die einzelnen Zeilen in einer Liste haben möchten, können wir die Funktion `lines` aus dem Modul `Data.List` verwenden, die den String in eine Liste von Strings aufteilt und dabei die Zeilenumbrüche ignoriert.

```Haskell
let linesList = lines contents
```

Jetzt können wir mit diesen Daten weiterarbeiten, beispielsweise sie bearbeiten, filtern oder in ein anderes Format umwandeln. Eine Möglichkeit wäre, die Buchstaben in jedem String in Großbuchstaben umzuwandeln und die Liste der Zeilen mit `unlines` wieder zu einem einzigen String zusammenzufügen.

```Haskell
let upperLines = map (map toUpper) linesList
let result = unlines upperLines
```

Zum Schluss müssen wir die Datei wieder schließen, damit sie nicht im Speicher bleibt und möglicherweise andere Anwendungen blockiert. Dazu verwenden wir die Funktion `hClose` und übergeben den geöffneten Handle.

```Haskell
hClose handle
```

Hier ist ein vollständiges Beispiel, das alle obigen Schritte zusammenfasst:

```Haskell
import System.IO
import Data.Char
import Data.List

openAndProcessFile :: FilePath -> IO ()
openAndProcessFile filePath = do
    handle <- openFile filePath ReadMode
    contents <- hGetContents handle
    let linesList = lines contents
    let upperLines = map (map toUpper) linesList
    let result = unlines upperLines
    putStrLn result
    hClose handle

main :: IO ()
main = do
    openAndProcessFile "beispiel.txt"
```

Die Funktion `openAndProcessFile` öffnet die Datei, liest den Inhalt, wandelt die Zeilen in Großbuchstaben um, gibt das Ergebnis in der Konsole aus und schließt schließlich die Datei. In der `main`-Funktion rufen wir diese Funktion mit dem gewünschten Dateipfad auf.

Das obige Beispiel ist sehr einfach gehalten und es gibt noch viele weitere Möglichkeiten, um Textdateien in Haskell zu lesen und zu bearbeiten. Hier sind einige weitere Ressourcen, die weitere Informationen und Beispiele bereithalten:

## Deep Dive

- [Haskell-Dokumentation zu System.IO](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
- [Haskell-Dokumentation zu Data.List](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html)
- [Haskell-Wiki-Artikel über Eingabe- und Ausgabefunktionen](https://wiki.haskell.org/Introduction_to_Input/Output)
- [Leitfaden zum Lesen und Schreiben von Textdateien in Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/reading-and-writing-files)

## Siehe auch

- [Haskell-Dokumentation zu `openFile`](https://hackage.h