---
title:                "Ein Textdokument lesen"
html_title:           "Elixir: Ein Textdokument lesen"
simple_title:         "Ein Textdokument lesen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Warum

Das Lesen von Textdateien ist eine häufige Aufgabe bei der Programmierung in Elixir. Ob Sie CSV-Daten analysieren oder Protokolldateien durchsuchen, das Lesen von Textdateien ist eine wichtige Fähigkeit, die Ihnen dabei helfen kann, komplexe Aufgaben zu lösen.

# Wie man es macht

Um eine Textdatei in Elixir zu lesen, können Sie die Funktion `File.read!/1` verwenden. Dieser Befehl liest die gesamte Datei als Binärdaten ein und gibt sie als String zurück.

```
Elixir
file = File.read!("beispiel.txt")
IO.puts(file)
```
Output:

```
Elixir
Dies ist eine Beispieldatei
Sie können beliebige Texte hier schreiben
Und sie werden alle in der Ausgabe angezeigt
```

Wenn Sie eine große Datei lesen möchten, können Sie die Funktion `File.stream!/1` verwenden, die die Datei als Strom von Zeilen liest und sie nacheinander zurückgibt.

```
Elixir
File.stream!("beispiel.txt")
|> Enum.each(&IO.puts/1)
```
Output:

```
Elixir
Dies ist eine Beispieldatei
Sie können beliebige Texte hier schreiben
Und sie werden alle in der Ausgabe angezeigt
```

# Tiefere Einblicke

Wenn Sie eine Textdatei mit speziellen Zeichensätzen lesen müssen, können Sie beim Lesen die Option `:encoding` verwenden. Zum Beispiel, wenn Ihre Datei im UTF-8 Format ist:

```
Elixir
file = File.read!("beispiel.txt", [:encoding, :utf8])
```

Sie können auch die Funktion `File.open!/2` verwenden, um eine Datei zu öffnen und sie Zugriff darauf für mehrere Operationen zu haben. Vergessen Sie nicht, die Datei am Ende zu schließen:

```
Elixir
file = File.open!("beispiel.txt", [:read])
IO.write(file, "Dies ist ein Beispieltext")
File.close(file)
```

# Siehe auch

- https://elixir-lang.org/getting-started/io-and-the-file-system.html
- https://hexdocs.pm/elixir/File.html