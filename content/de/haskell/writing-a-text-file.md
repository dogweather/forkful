---
title:                "Eine Textdatei schreiben."
html_title:           "Haskell: Eine Textdatei schreiben."
simple_title:         "Eine Textdatei schreiben."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Textdateien sind ein grundlegendes Werkzeug in der Programmierung und werden häufig verwendet, um Informationen zu speichern und auszutauschen. Durch das Schreiben einer Textdatei in Haskell lernen wir nicht nur, wie man grundlegende Daten manipuliert, sondern auch, wie man mit Dateien interagiert, was in der modernen Softwareentwicklung unerlässlich ist.

## Wie es geht

Um eine Textdatei in Haskell zu schreiben, müssen wir zuerst die Funktion `writeFile` aus der `System.IO` Bibliothek importieren. Diese Funktion akzeptiert zwei Parameter: den Dateipfad und den Inhalt, den wir in die Datei schreiben möchten. In unserem Beispiel möchten wir den String "Hallo Welt!" in die Datei "beispiel.txt" schreiben. Dazu schreiben wir folgenden Code:

```Haskell
import System.IO

main = do
  writeFile "beispiel.txt" "Hallo Welt!"
```

Nach dem Ausführen dieses Programms wird die Datei "beispiel.txt" erstellt und der String "Hallo Welt!" wird in die Datei geschrieben.

## Tiefer Einblick

Um einen tieferen Einblick in das Schreiben von Textdateien in Haskell zu bekommen, können wir uns die Quellcodeimplementierung von `writeFile` ansehen. Diese befindet sich in der Datei "Text\IO.hs" in der `System.IO` Bibliothek. Dort sehen wir, dass `writeFile` intern die Funktion `withFile` verwendet, die es uns ermöglicht, eine Datei zu öffnen, darauf zuzugreifen und sie dann automatisch zu schließen. Dies ist ein gutes Beispiel für das funktionale Programmierparadigma, bei dem Funktionen als Parameter an andere Funktionen übergeben werden.

## Siehe auch

- [Haskell Dokumentation zu Dateioperationen](https://www.haskell.org/onlinereport/io.html)
- [Haskell Bibliothek "System.IO"](https://hackage.haskell.org/package/base-4.14.0.0/docs/System-IO.html)