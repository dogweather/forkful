---
title:    "Elixir: Verfassen einer Textdatei"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich überhaupt damit beschäftigen, eine Textdatei zu schreiben? Nun, Textdateien sind ein wesentlicher Bestandteil der Programmierung. Sie dienen als einfache und effiziente Möglichkeit, Daten zu speichern und zu verarbeiten. Mit Elixir können wir schnell und einfach Textdateien erstellen und bearbeiten, was uns in unseren Projekten viel Zeit und Mühe erspart.

## Wie geht's

Um eine Textdatei in Elixir zu schreiben, können wir die Funktion `File.write/2` verwenden. Hier ist ein Beispielcode, der eine Textdatei erstellt und einen beliebigen Text hineinschreibt:

```Elixir
File.write("meine_textdatei.txt", "Hallo, dies ist ein Beispieltext!")
```

Nach dem Ausführen dieses Codes wird eine neue Textdatei mit dem Namen "meine_textdatei.txt" erstellt und der Text "Hallo, dies ist ein Beispieltext!" darin gespeichert. Wir können auch bestehende Textdateien öffnen und bearbeiten, indem wir die Funktion `File.open/2` verwenden. Hier ist ein Beispiel, das eine bestehende Textdatei öffnet, den Inhalt liest und auf der Konsole ausgibt:

```Elixir
File.open("meine_textdatei.txt")
|> IO.read()
|> IO.puts()
```

Der Output dieser Codebeispiel ist "Hallo, dies ist ein Beispieltext!". Wir können auch Textdateien mit Hilfe von Elixir Streams bearbeiten, was uns die Möglichkeit gibt, große Dateien zu verarbeiten, ohne den gesamten Inhalt in den Arbeitsspeicher zu laden. Es gibt viele weitere Möglichkeiten, wie wir Textdateien in Elixir erstellen und bearbeiten können. Schaue dir die Links in der "Siehe auch" Sektion unten an, um mehr darüber zu erfahren.

## Tiefgehende Informationen

Neben `File.write/2` und `File.open/2` gibt es in Elixir noch andere nützliche Funktionen zum Schreiben und Lesen von Textdateien. Zum Beispiel können wir `File.read/1` verwenden, um den gesamten Inhalt einer Datei in eine Variable zu speichern, oder `File.stream!/2`, um eine Elixir Stream aus einer Textdatei zu erstellen. Wir können auch Dateien mit unterschiedlichen Encodings lesen und schreiben, indem wir die entsprechenden Optionen in den Funktionen `File.open/2` und `File.write/2` angeben. Es lohnt sich, sich ausführlicher mit den verschiedenen Funktionen und Möglichkeiten von Elixir auseinanderzusetzen, um Textdateien effizient zu bearbeiten.

## Siehe auch

- [Elixir Dokumentation zu Textdateien](https://hexdocs.pm/elixir/File.html)
- [Eine Einführung in Elixir Streams](https://elixirschool.com/de/lessons/advanced/streams/)
- [Codierungsoptionen der ExFile Bibliothek](https://github.com/parroty/exfile#encoding--decoding-options)