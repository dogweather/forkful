---
title:                "Einlesen einer Textdatei"
html_title:           "Elm: Einlesen einer Textdatei"
simple_title:         "Einlesen einer Textdatei"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Am Anfang jeder Programmiererlaufbahn steht die Aufgabe, Daten aus einer Textdatei zu lesen. Egal, ob du deine erste Hello-World-Anwendung schreibst oder an einem komplexen Softwareprojekt arbeitest, die Fähigkeit, eine Textdatei einzulesen, ist unerlässlich.

## How To

Um eine Textdatei in Elm zu lesen, benötigst du das Paket "elm/file". Füge zunächst die Abhängigkeit in deine elm.json Datei hinzu:

```
"dependencies": {
    "elm/file": "1.0.2"
}
```

Importiere dann das Modul in deinem Code:

```
import File exposing (readFile)
```

Nun kannst du die Funktion `readFile` verwenden, um eine Textdatei einzulesen. Sie gibt ein `Task` Objekt zurück, das eine Liste mit Zeilen aus der Datei enthält. Mit Hilfe von `andThen` kannst du darauf zugreifen und die Daten weiterverarbeiten:

```
readFile "meine_datei.txt"
    |> Task.andThen (\task ->
        case task of
            Err error ->
                -- Fehlerbehandlung
                
            Ok lines ->
                -- Datenverarbeitung
    )
```

## Deep Dive

Die Funktion `readFile` akzeptiert zwei Argumente: einen Dateipfad als `String` und eine Konfigurationsliste als `List`. Die Konfigurationsoptionen umfassen unter anderem die Codierung der Datei und ob die Daten als `String` oder `Bytes` zurückgegeben werden sollen.

Wenn du eine Datei mit speziellen Zeichensätzen wie UTF-8 öffnen möchtest, kannst du dies in der Konfigurationsliste angeben:

```
[String "UTF-8"] |> readFile "meine_datei.txt"
```

Du kannst auch die Daten als `Bytes` statt als `String` zurückgeben lassen. Dies kann besonders hilfreich sein, wenn du binäre Dateien liest:

```
bytesCfg = [ String "UTF-8", FormatBytes ] |> readFile "meine_datei.bin"
```

## Siehe auch

- Offizielle Dokumentation zu [elm/file](https://package.elm-lang.org/packages/elm/file/latest/)
- Beispielprojekt zum Einlesen von Textdateien in [Elm in Action](https://livebook.manning.com/book/elm-in-action/chapter-7/)