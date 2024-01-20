---
title:                "Teilzeichenketten extrahieren"
html_title:           "PowerShell: Teilzeichenketten extrahieren"
simple_title:         "Teilzeichenketten extrahieren"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was und Warum?

Das Extrahieren von Teilstrings (substrings) bedeutet, bestimmte Teile einer Zeichenkette auszuwählen und zu isolieren. Programmierer machen dies, um Informationen zu analysieren, zu filtern oder zu manipulieren.

## So geht's:

In Elixir können wir die Funktion `String.slice/3` verwenden, um Teilstrings zu extrahieren. Hier ist ein einfaches Beispiel:

```elixir
str = "Elixir ist toll"
substring = String.slice(str, 0, 6)
IO.puts substring # Ausgabe: "Elixir"
```

Ein weiteres Beispiel zeigt, wie man den letzten Teil einer Zeichenkette extrahiert:

```elixir
str = "Elixir ist toll"
substring = String.slice(str, -4, 4)
IO.puts substring # Ausgabe: "toll"
```

## Deep Dive

Die Extraktion von Teilstrings ist eine grundlegende Funktion jedes modernen Programmiersprachen. In Elixir wird sie durch das `:binary` Modul ermöglicht, das die binäre Datenstruktur der erlang Maschine nutzt.

Alternativ zur `String.slice/3` Methode, können Sie auch die `:binary.part/2` Funktion verwenden. Sie kann etwas flexibler sein, erfordert aber ein bisschen mehr Code.

In Sachen Implementierung wird das Extrahieren von Teilstrings in Elixir tatsächlich sehr effizient ausgeführt. Dies liegt größtenteils an der Art und Weise, wie erlang Binärdateien handhabt. Statt eine Kopie des Teilstrings zu erstellen, behält es einfach einen Verweis auf den ursprünglichen String und verschiebt die Start- und/oder Endzeiger.

## Siehe auch

- Elixir offizielle Dokumentation: [String](https://hexdocs.pm/elixir/String.html) und [:binary](http://erlang.org/doc/man/binary.html) Modul
- Erlang Dokumentation: [Einführung in Binärdateien](http://erlang.org/doc/programming_examples/bit_syntax.html)
- Artikel: [Effiziente Zeichenkettenmanipulation in Erlang](https://medium.com/@jlouis666/efficient-string-handling-in-erlang-2d7e65ab0143)