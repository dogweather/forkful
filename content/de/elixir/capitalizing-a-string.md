---
title:                "Einen String großschreiben"
html_title:           "Elixir: Einen String großschreiben"
simple_title:         "Einen String großschreiben"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Ein String zu kapitalisieren bedeutet, den ersten Buchstaben des Strings in ein Großbuchstaben zu verwandeln. Programmierer machen das, um die Lesbarkeit und Klarheit des Ausdrucks zu verbessern.

## So geht's:

```Elixir
String.capitalize("elixir")
```

Die Ausgabe des obigen Befehls wird "Elixir" sein.

```Elixir
IO.puts String.capitalize("elixir")
```

Die Ausgabe des obigen Befehls am Elixir-Eingabeaufforderung wird "Elixir" sein.

## Tiefgreifend:

Kapitalisierung, so wie wir sie kennen, hat in den Tagen von Telex-Druckern begonnen, die nur Großbuchstaben drucken konnten. Im Elixir findet die String-Kapitalisierung durch die `String.capitalize/1` Funktion statt, die intern den `:unicode` Modul nutzt. Eine Alternative wäre die Verwendung von `String.upcase/1` auf den ersten Buchstaben des Strings und das Anhängen des restlichen, unveränderten Strings.

## Siehe auch:

- [Elixir String API Documentation](https://hexdocs.pm/elixir/String.html#capitalize/1)
- [Chat Elixir for beginners](https://elixir-lang.org/getting-started/introduction.html) 
- [Elixir on Github](https://github.com/elixir-lang/elixir)