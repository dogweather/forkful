---
title:                "Überprüfung, ob ein Verzeichnis existiert"
html_title:           "Elixir: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?
In der Programmierung prüft man, ob ein Verzeichnis existiert ("Checking if a directory exists"), um Fehler aufgrund fehlender Verzeichnisse zu vermeiden. Das schützt Ihre Codeausführung vor unerwarteten Unterbrechungen.

## So geht's:
Mit Elixir können Sie die `File.dir?/1` Funktion verwenden, um zu überprüfen, ob ein Verzeichnis existiert. Hier ist eine einfache Demonstration:

```elixir
defmodule Test do
  def dir_exists?(dir) do
    File.dir?(dir)
  end
end

IO.puts Test.dir_exists?("/path/to/directory")  # Dies könnte entweder wahr oder falsch zurückgeben, je nachdem, ob das Verzeichnis existiert.
```

## Vertiefung
Die Funktion `File.dir?/1` wurde erstmals in Elixir 1.0 eingeführt und ist seitdem unverändert geblieben. Es gibt Alternativen wie das Verwenden von `:filelib.file_info/1` aus Erlang, aber `File.dir?/1` ist die einfachste und am häufigsten bevorzugte Methode in Elixir. Intern verwendet `File.dir?/1` tatsächlich die Erlang Funktion :filelib.isDirectory/1, um die Existenz eines Verzeichnisses zu überprüfen.

## Siehe Auch
1. Offizielle Elixir Dokumentation zu `File.dir? / 1` - https://hexdocs.pm/elixir/File.html#dir%3F/1
2. Erlang Dokumentation zu `:filelib.file_info/1` - https://erlang.org/doc/man/filelib.html#file_info-1
3. Elixir Forum Diskussionen zum Überprüfen der Existenz eines Verzeichnisses - https://elixirforum.com/t/how-to-check-if-a-directory-exists/1203