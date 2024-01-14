---
title:    "Elixir: Eine Textdatei schreiben"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Warum

Das Schreiben von Textdateien kann eine nützliche Fähigkeit in der Elixir-Programmierung sein. Es ermöglicht es uns, Daten in einer einfach lesbaren Form zu speichern und zu manipulieren. Dadurch können wir unsere Programme flexibler und effizienter gestalten.

## Wie geht das?

Wir können in Elixir ganz einfach eine Textdatei schreiben, indem wir die Funktion `File.write/2` verwenden. Hier ist ein Beispielcode, der einen Text in eine Datei namens "beispiel.txt" schreibt:

```elixir
File.write("beispiel.txt", "Dies ist ein Beispieltext.")
```

Wenn wir diesen Code ausführen, wird in unserem Programmverzeichnis eine neue Textdatei mit dem Inhalt "Dies ist ein Beispieltext." erstellt. Wir können auch Variablen verwenden, um dynamische Daten in die Datei zu schreiben. Zum Beispiel:

```elixir
name = "Max"

File.write("beispiel.txt", "Hallo {name}!")
```

Dies wird "Hallo Max!" in die Datei schreiben. Wir können auch mehrere Zeilen in einer Datei schreiben, indem wir die Funktion `IO.write/2` verwenden und jedes Mal eine neue Zeile beginnen. Zum Beispiel:

```elixir
File.write("beispiel.txt", "Erste Zeile.\n")
File.write("beispiel.txt", "Zweite Zeile.")
```

Verwenden Sie das Escape-Zeichen `\n`, um eine neue Zeile einzufügen. Das Ergebnis wird in der beispiel.txt-Datei wie folgt aussehen:

```
Erste Zeile.
Zweite Zeile.
```

## Tieferer Einblick

Wir können auch verschiedene Optionen angeben, um unsere Textdateien in Elixir zu formatieren. Zum Beispiel können wir die Codierung, die Zeilenendungen und die Rechte beim Erstellen der Datei angeben. Hier ist ein Beispielcode:

```elixir
options = [encoding: :utf8, newline: :lf, mode: :read_write]

File.write("beispiel.txt", "Dies ist ein Beispieltext.", options)
```

Sie können diese Optionen an Ihre spezifischen Bedürfnisse anpassen.

## Siehe auch

- Offizielle Dokumentation für `File.write/2`: https://hexdocs.pm/elixir/File.html#write/2
- "How to Write a Text File in Elixir" von Bright Inventions: https://brightinventions.pl/blog/write-text-file-in-elixir/
- "Elixir File Processing: Writing Files" von Abialbon: https://abialbon.com/elixir-file-processing-writing-files/