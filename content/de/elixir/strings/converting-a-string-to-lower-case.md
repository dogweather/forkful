---
date: 2024-01-20 17:38:15.380096-07:00
description: 'How to: Elixir macht die Konvertierung mit der `String.downcase/1` Funktion
  kinderleicht. Hier ist ein Beispiel.'
lastmod: '2024-03-13T22:44:53.449882-06:00'
model: gpt-4-1106-preview
summary: Elixir macht die Konvertierung mit der `String.downcase/1` Funktion kinderleicht.
title: Umformung eines Strings in Kleinbuchstaben
weight: 4
---

## How to:
Elixir macht die Konvertierung mit der `String.downcase/1` Funktion kinderleicht. Hier ist ein Beispiel:

```elixir
original = "Elixir ROCKT!"
klein = String.downcase(original)
IO.puts(klein)  # Gibt aus: "elixir rockt!"
```

Dieser Code wandelt den String `original` in Kleinbuchstaben um und gibt ihn aus.

## Deep Dive
Früher war die Umwandlung von Groß- zu Kleinbuchstaben nicht so geradlinig, da es in verschiedenen Sprachen unterschiedliche Regeln gibt. Elixir nutzt Unicode, wodurch die meisten sprachspezifischen Fälle abgedeckt sind.

Eine Alternative könnte eine manuelle Konvertierung mit einer Schleife und Charaktervergleich sein, aber dies wäre unwirtschaftlich und fehleranfällig. Da Elixir auf der Erlang VM läuft, nutzt es womöglich auch Erlangs robuste String-Handling-Funktionen.

Bei der Implementierung ist wichtig, dass `String.downcase/1` auch mit Graphemen umgehen kann, die aus mehreren Unicode-Zeichen bestehen. So werden auch kombinierte Charaktere richtig in Kleinbuchstaben umgewandelt.

## See Also
- Elixir Documentation für `String.downcase/1`: https://hexdocs.pm/elixir/String.html#downcase/1
- Unicode Standard: https://unicode.org
- Erlang's String-Module, welches von Elixir verwendet wird: http://erlang.org/doc/man/string.html
