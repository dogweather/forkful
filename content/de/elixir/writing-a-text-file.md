---
title:                "Elixir: Schreiben einer Textdatei"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Schreiben von Textdateien ist ein grundlegender Teil der Programmierung in Elixir. Durch das Erstellen und Lesen von Textdateien können wir Daten organisieren, speichern und nutzen. Es ermöglicht uns auch, Informationen zwischen verschiedenen Programmen auszutauschen.

## Wie geht man vor

Um eine Textdatei in Elixir zu schreiben, können wir die `File.write/2` Funktion verwenden. Zuerst müssen wir eine Dateipfad angeben, in der die Datei erstellt werden soll, gefolgt von dem Inhalt, den wir in die Datei schreiben möchten. Beispiel:

```elixir
File.write("meine_datei.txt", "Hallo Welt!")
```

Dieser Code erstellt eine neue Textdatei namens "meine_datei.txt" und schreibt den Inhalt "Hallo Welt!" in die Datei. Wenn wir den Inhalt der Datei überprüfen möchten, können wir die `File.read/1` Funktion verwenden. Beispiel:

```elixir
File.read("meine_datei.txt")

# Output: "Hallo Welt!"
```

Wir können auch mehrere Zeilen in eine Textdatei schreiben, indem wir den Inhalt mit einem Zeilenumbruch trennen. Beispiel:

```elixir
File.write("meine_datei.txt", "Dies ist Zeile 1 \nDas ist Zeile 2")
```

Dies erstellt eine Textdatei mit zwei Zeilen Inhalt.

## Tiefentauchen

Um die Funktionsweise von Textdateien in Elixir besser zu verstehen, ist es hilfreich, sich mit dem binären Datentyp vertraut zu machen. Elixir behandelt Textdateien als Binärdaten, weshalb wir auch die `File.read/1` Funktion verwenden können, um den Inhalt als Binärdaten zurückzugeben. Wir können auch die Länge der Datei mit der `File.read!/1` Funktion abrufen. Beispiel:

```elixir
file = File.read("meine_datei.txt")
byte_size(file) # Gibt die Anzahl der Bytes in der Datei zurück
```

Wir können auch eine Textdatei mit binären Daten erstellen und schreiben. Beispiel:

```elixir
file = File.open("meine_datei.txt", [:write])
IO.binwrite(file, <<33, 44, 55>>) # Schreibt die binären Daten in die Datei
```

Um mehr über die Arbeit mit Binärdaten in Elixir zu erfahren, schaut euch die offizielle Dokumentation dazu an.

## Siehe auch

- [Offizielle Elixir-Dokumentation zu Dateioperationen](https://hexdocs.pm/elixir/File.html)
- [Erlang-Binärimplementierung in Elixir](https://hexdocs.pm/elixir/1.11.2/IO.html)
- [Elixir-Forum für Fragen und Diskussionen](https://elixirforum.com/)