---
title:                "Eine temporäre Datei erstellen"
html_title:           "Elm: Eine temporäre Datei erstellen"
simple_title:         "Eine temporäre Datei erstellen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Erstellen einer temporären Datei ist ein gängiger Prozess in der Programmierung. Temporäre Dateien dienen als Zwischenspeicher für Daten, die während der Ausführung eines Programms benötigt werden. Sie werden oft verwendet, um große Datenmengen zwischenzuspeichern, die den Arbeitsspeicher überlasten könnten.

## Wie geht's?
Eine temporäre Datei in Elm zu erstellen ist einfach. Hier ist ein Beispielcode, der eine temporäre Datei erstellt und ihren Namen auf der Konsole ausgibt:

```
Elm Tempfile Beispiel

import Tempfile exposing (..)

main = 
    let 
        { success, name } = tempfile "meine_temp_datei" ".txt" 
    in 
        if success 
            then 
                putStrLn ("Name der Temporären Datei: " ++ name) 
            else 
                putStrLn "Fehler beim Erstellen der Temporären Datei"
```

Die Ausgabe dieses Codes wäre:

```
Name der Temporären Datei: meine_temp_datei.txt
```

## Tiefer Einblick
Das Konzept der temporären Dateien entstand in den 1960er Jahren mit der Entwicklung von Betriebssystemen, um eine effiziente Verwaltung von Speicherressourcen zu ermöglichen. In Elm gibt es auch andere Möglichkeiten, temporäre Dateien zu erstellen, wie z.B. mit der `Filesystem`-Bibliothek. Diese bietet weitere Funktionen für die Verwaltung von Dateien und Ordnern.

## Sieh dir auch an
- Die offizielle Elm-Dokumentation zu `Tempfile`: https://package.elm-lang.org/packages/elm-lang/core/latest/Tempfile 
- Ein Tutorial zum Erstellen von temporären Dateien mit der `Filesystem`-Bibliothek: https://elmprogramming.com/tutorials/create-temporary-file-in-elm.html