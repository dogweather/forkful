---
title:                "Elixir: Ein Textdokument schreiben"
simple_title:         "Ein Textdokument schreiben"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Das Erstellen von Textdateien ist ein wichtiger Bestandteil der Programmierung. Es ermöglicht uns, Daten und Informationen in einer leicht lesbaren Form zu speichern und zu organisieren. Dies ist besonders nützlich, wenn wir mit größeren Datenmengen arbeiten oder Informationen für spätere Verwendung speichern möchten.

## Wie geht das?

Um eine Textdatei in Elixir zu erstellen, verwenden wir die Funktion `File.write/2` und geben den Dateinamen als ersten Parameter und den Inhalt als zweiten Parameter an. Zum Beispiel:

```Elixir
File.write("beispieldatei.txt", "Dies ist eine Beispieldatei!")
```

Dieser Code erstellt eine neue Datei mit dem Namen "beispieldatei.txt" und schreibt den gegebenen Text in die Datei. Wenn wir die Datei öffnen, werden wir den Text sehen.

Wir können auch Variablen verwenden, um den Inhalt der Datei dynamischer zu gestalten. Zum Beispiel:

```Elixir
name = "Max Mustermann"
age = 25
File.write("info.txt", "Name: #{name}\nAlter: #{age}")
```
Dieser Code erstellt eine Datei mit dem Namen "info.txt" und schreibt den Inhalt "Name: Max Mustermann Alter: 25" in die Datei. Beachten Sie, dass wir den Zeilenumbruch `\n` verwendet haben, um auf eine neue Zeile zu springen.

## Tiefentauchen

Beim Schreiben von Textdateien gibt es einige Dinge zu beachten. Zum einen müssen wir sicherstellen, dass die Datei, die wir erstellen möchten, nicht bereits existiert, da dies zu einer Fehlermeldung führen würde. Wir können dies überprüfen, indem wir die Funktion `File.exists?/1` verwenden:

```Elixir
if File.exists?("neue_datei.txt") do
  IO.puts "Datei existiert bereits!"
else
  File.write("neue_datei.txt", "Dies ist eine neue Datei!")
end
```

Zum anderen sollten wir sicherstellen, dass wir die Datei am Ende unseres Programms schließen, um mögliche Speicherlecks zu vermeiden. Dafür können wir die Funktion `File.close/1` verwenden:

```Elixir
file = File.open("notizen.txt")
# tu etwas mit der Datei
File.close(file)
```

Wenn wir bereits eine Datei haben und lediglich zusätzliche Informationen hinzufügen möchten, können wir die Funktion `File.append/2` verwenden. Diese funktioniert ähnlich wie `File.write/2`, schreibt jedoch zusätzliches Material am Ende der Datei anstatt sie zu überschreiben.

## Siehe auch

- [Elixir Dokumentation zu File](https://hexdocs.pm/elixir/File.html)
- [Elixir Tutorial - Dateien erstellen und schreiben](https://elixir-lang.org/getting-started/io-and-the-file-system.html#creating-and-writing-to-files)