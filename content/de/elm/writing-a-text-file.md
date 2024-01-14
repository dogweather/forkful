---
title:                "Elm: Das Verfassen einer Textdatei"
simple_title:         "Das Verfassen einer Textdatei"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben von Textdateien ist ein grundlegender Teil der Programmierung in Elm. Durch das Erstellen von Textdateien können wir Informationen speichern, organisieren und jederzeit wieder abrufen. Dies ist besonders hilfreich, wenn wir mit großen Datenmengen arbeiten oder unser Programm mit externen Anwendungen integrieren möchten.

## Wie geht es?

Um eine Textdatei in Elm zu schreiben, müssen wir zunächst die "text" Funktion aus dem "elm/core"-Paket importieren. Dann können wir diese Funktion verwenden, um den Textinhalt zu erstellen und in eine Datei zu schreiben. Hier ist ein Beispielcode:

```
import Text exposing (text)

main =
    text "Dies ist ein Beispieltext."
```

Das Ergebnis wird in einer Datei namens "output.txt" gespeichert. Wir können auch angeben, in welchen Ordner die Datei gespeichert werden soll, indem wir den Dateipfad zusammen mit dem Dateinamen angeben:

```
import Text exposing (text)

main =
    text "Dies ist ein Beispieltext."
        
outputFile = "/Users/MyName/Desktop/output.txt"
```

Im obigen Beispiel wird die Datei im Ordner "Desktop" unter dem Dateinamen "output.txt" gespeichert. Natürlich können wir auch einen dynamischen Dateinamen erstellen, indem wir Variablen verwenden.

## Tiefer Einblick

Die "text" Funktion allein ermöglicht es uns, Text in eine Datei zu schreiben. Aber in manchen Fällen möchten wir vielleicht auch zusätzliche Informationen wie Zahlen oder Datentypen in unsere Datei einfügen. Hier können wir die Funktion "toString" verwenden, um unsere Daten in einen String umzuwandeln, der dann von der "text" Funktion verwendet werden kann. Zum Beispiel:

```
import Text exposing (text)

main =
    let
        number = 42
    in
        text (toString number)
```

Dies wird die Zahl "42" in die Datei schreiben. Wir können auch komplexere Datentypen wie Listen oder benutzerdefinierte Typen in Strings umwandeln und dann in die Datei schreiben.

## Siehe auch

* [Elm Dokumentation zur text Funktion](https://package.elm-lang.org/packages/elm/core/latest/Text)
* [Tutorial zur Dateiverwaltung in Elm](https://www.elm-tutorial.org/en/03-subs-cmds/05-command.html)
* [Beispielprojekt zum Schreiben von Textdateien mit Elm](https://github.com/elm/projects/tree/master/write-text-file)