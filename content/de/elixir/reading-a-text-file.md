---
title:                "Elixir: Textdateien lesen"
simple_title:         "Textdateien lesen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit dem Lesen von Textdateien in Elixir beschäftigen? Nun, Textdateien sind ein grundlegender Bestandteil der Datenverarbeitung und können in vielen Anwendungen nützlich sein, sei es für die Eingabe von Benutzereingaben, das Lesen von externen Konfigurationsdateien oder das Analysieren von Logfiles.

## Wie man Textdateien in Elixir liest

Um eine Textdatei in Elixir zu lesen, müssen wir zunächst die Funktion `File.read/1` verwenden, die uns ermöglicht, einen Dateinamen als Parameter zu übergeben und den Inhalt der Datei als Binärdaten zurückzugeben. Hier ist ein Beispiel:

```Elixir
{:ok, content} = File.read("mein_textdokument.txt")
IO.puts(content)
```

In diesem Codebeispiel speichern wir den Inhalt des Dokuments in einer Variablen namens `content` und geben ihn dann mit `IO.puts/1` auf der Konsole aus.

## Tiefere Einblicke

Beim Lesen von Textdateien gibt es einige wichtige Dinge zu beachten. Erstens, wenn die Datei sehr groß ist, kann es sinnvoll sein, sie zeilenweise oder in Chunks zu lesen, um den Speicherverbrauch zu minimieren. Dies kann mit der Funktion `File.stream!/2` erreicht werden, die uns einen Stream von Daten aus der Datei zurückgibt.

Zweitens, beim Lesen von Dateien, die in verschiedenen Zeichensätzen codiert sein können, ist es wichtig, den korrekten Zeichensatz beim Öffnen der Datei anzugeben. Dies kann mit der Option `{:encoding, encoding_name}` beim Aufruf der Funktion `File.read/2` gemacht werden.

## Siehe auch

- Offizielle Elixir Dokumentation für File.read: https://hexdocs.pm/elixir/File.html#read/1
- Einführung in das Lesen von Textdateien in Elixir: https://medium.com/@ericdecanini/how-to-read-a-text-file-with-elixir-5a850aabe4aa
- Lesen von großen Dateien in Elixir: https://medium.com/@robocopkaka/reading-large-files-line-by-line-in-elixir-e2d6cdca9c61