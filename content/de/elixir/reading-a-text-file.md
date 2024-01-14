---
title:                "Elixir: Textdatei lesen"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum man sich mit dem Lesen von Textdateien in Elixir beschäftigen sollte. Zum einen ist es ein grundlegendes Konzept, das häufig in der Programmierung vorkommt. Zum anderen kann es bei der Entwicklung von Dateiverarbeitungs- oder Datenbankanwendungen von entscheidender Bedeutung sein. Egal aus welchem ​​Grund, das Lesen von Textdateien ist eine nützliche Fähigkeit, die jeder Elixir-Programmierer kennen sollte.

## Wie man Textdateien in Elixir liest

Das Lesen von Textdateien in Elixir ist einfach und effizient. Zunächst müssen wir eine Textdatei öffnen und den Inhalt in einer Variablen speichern. Dies kann mit der `File.read` Funktion erreicht werden, die den Dateipfad als Argument nimmt. Schauen wir uns das in einem Codebeispiel an:

```Elixir
file = File.read("mein_textdokument.txt")
IO.puts(file)
```

In diesem Beispiel öffnen wir die Datei "mein_textdokument.txt" und speichern den Inhalt in der Variablen `file`. Anschließend geben wir den Inhalt mit `IO.puts` aus, um ihn auf der Konsole anzuzeigen. Diese Funktion funktioniert auch für andere Dateiformate wie CSV oder JSON.

## Tiefer eintauchen

Um tiefer in das Lesen von Textdateien in Elixir einzutauchen, ist es wichtig zu verstehen, dass `File.read` den Inhalt der Datei als Binärdaten zurückgibt. Um also mit dem Inhalt zu arbeiten, müssen wir ihn möglicherweise in einen String oder eine Liste umwandeln. Dies kann mit den Funktionen `to_string` und `String.split` erreicht werden. Darüber hinaus bietet Elixir auch integrierte Funktionen wie `File.exists?` und `File.read!`, um auf die Datei zuzugreifen und Fehler zu behandeln.

## Siehe auch

- Offizielle Elixir-Dokumentation zu File-Modulen: https://hexdocs.pm/elixir/File.html
- Lesen von Dateien mit Elixir: https://thinkingelixir.com/reading-files-with-elixir/
- Blog-Post zum Schreiben von Textdateien mit Elixir: https://blog.lelonek.me/reading-and-writing-files-in-elixir-2f12b0b7b9ea