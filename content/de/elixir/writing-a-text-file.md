---
title:                "Eine Textdatei schreiben"
html_title:           "Arduino: Eine Textdatei schreiben"
simple_title:         "Eine Textdatei schreiben"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben einer Textdatei ist das Speichern von Daten in einem lesbaren Format auf einem Speichermedium. Programmierer tun dies, um Daten für eine spätere Verwendung zu persistieren, Konfigurationen zu speichern, oder Logs zu protokollieren.

## How to:
Schreiben einer simplen Textdatei mit Elixir:

```elixir
File.write("hello.txt", "Hallo Welt!")
```

Ausgabe prüfen:
```elixir
File.read("hello.txt")
```

Erwartete Ausgabe:
```elixir
{:ok, "Hallo Welt!"}
```

Stream zum Schreiben von größeren Dateien:

```elixir
{:ok, file} = File.open("große_datei.txt", [:write])
IO.binwrite(file, "Erste Zeile\n")
IO.binwrite(file, "Zweite Zeile\n")
File.close(file)
```

## Deep Dive
Historisch stammt das Konzept des Schreibens von Daten in Dateien vom Bedarf der dauerhaften Datenspeicherung ab. Alternativen zu `File.write/2` sind Streams für größere Daten und Datenbanken für strukturierte Daten. Implementation in Elixir verwendet Erlang's :file Modul, wodurch es effizient und zuverlässig ist.

## See Also
- Elixir's offizielle Dokumentation: [File Module](https://hexdocs.pm/elixir/File.html)
- Erlang's :file Modul: [Erlang File Module](http://erlang.org/doc/man/file.html)
- Elixir Forum für Diskussionen und Fragen: [Elixir Forum](https://elixirforum.com)