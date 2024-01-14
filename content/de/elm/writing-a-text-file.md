---
title:    "Elm: Schreiben einer Textdatei"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben von Textdateien ist eine wichtige Fähigkeit für jeden Programmierer. Mit Elm, einer funktionalen Programmiersprache, ist es einfach und effizient, Textdateien zu erstellen. In diesem Blog-Beitrag werden wir uns ansehen, warum das Schreiben von Textdateien in Elm wichtig ist und wie man es am besten macht.

## Wie geht es

Um eine Textdatei in Elm zu schreiben, verwenden wir die Funktion `File.write`. In diesem Codebeispiel erstellen wir eine Datei mit dem Namen "HalloWelt.txt" und schreiben den Text "Hallo Welt!" hinein.

```
Elm File.write "HalloWelt.txt" "Hallo Welt!"
```

Die Ausgabe dieses Codes wird in der Datei "HalloWelt.txt" gespeichert und der Inhalt wird "Hallo Welt!" sein.

## Tiefer eintauchen

Es gibt viele weitere Möglichkeiten, Textdateien mit Elm zu schreiben. Wir können zum Beispiel die Funktion `File.append` verwenden, um Text an eine bereits bestehende Datei anzuhängen. Wir können auch die Funktionen `File.read` und `File.readLines` verwenden, um den Inhalt einer Textdatei in unser Programm einzulesen. Mit diesen Funktionen können wir Dateien effizient verarbeiten und nutzen.

Weitere Informationen zur Arbeit mit Textdateien in Elm finden Sie in der offiziellen Elm-Dokumentation.

## Siehe auch

- [Elm Documentation on File I/O](https://guide.elm-lang.org/io/files.html)
- [Elm-Lang Nutzerforum](https://discourse.elm-lang.org/)
- [Elm Blog](https://elm-lang.org/blog)