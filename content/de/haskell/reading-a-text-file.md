---
title:    "Haskell: Einen Textdatei lesen"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Warum Lesen einer Textdatei

Das Lesen von Textdateien ist eine grundlegende Fähigkeit beim Programmieren. Es erlaubt uns, Daten aus externen Quellen zu importieren und sie in unsere Programme zu integrieren. In diesem Blog Post werde ich zeigen, wie wir Textdateien in Haskell lesen können und welche Vorteile es mit sich bringt.

## Wie man eine Textdatei liest

Zunächst müssen wir eine Textdatei erstellen, die wir lesen möchten. Dazu können wir in Haskell die `writeFile` Funktion verwenden, die eine Datei mit dem angegebenen Namen und Inhalt erstellt.

```Haskell
writeFile "datei.txt" "Dies ist der Inhalt der Datei."
```

Nun können wir die Datei mit der Funktion `readFile` einlesen und den Inhalt anzeigen lassen.

```Haskell
main = do
    content <- readFile "datei.txt"
    putStrLn content
```

Dies werden wir in einem beliebigen Texteditor speichern und mit `runhaskell` ausführen. Der Inhalt der Datei wird dann in der Konsole ausgegeben.

## Tiefere Einblicke

Beim Lesen einer Textdatei in Haskell können wir auch zusätzliche Funktionen verwenden, um den Inhalt zu verarbeiten. Zum Beispiel können wir die `lines` Funktion verwenden, um den Inhalt in separate Zeilen zu unterteilen. Oder die `words` Funktion, um den Text in einzelne Wörter zu zerlegen.

```Haskell
main = do
    content <- readFile "datei.txt"
    let lines = lines content -- unterteilt den Inhalt in Zeilen
    print lines -- gibt die einzelnen Zeilen aus
```

Zudem können wir auch Filterfunktionen verwenden, um beispielsweise nur bestimmte Zeilen auszugeben oder zu verwerfen.

## Siehe auch

- [Haskell Dokumentation zum Lesen von Dateien](https://www.haskell.org/tutorial/io.html#files-and-i-o)
- [Einführung in die Haskell Programmierung](https://www.geeksforgeeks.org/introduction-to-haskell-programming-language/)
- [Haskell Programmierbeispiele von Project Euler](https://projecteuler.net/problem=1)