---
date: 2024-01-20 17:50:47.280284-07:00
description: 'How to: Elixir macht das Verwenden von String-Interpolation leicht mit
  dem `#{}` Mechanismus. Hier ein paar Beispiele.'
lastmod: '2024-03-13T22:44:53.448979-06:00'
model: gpt-4-1106-preview
summary: Elixir macht das Verwenden von String-Interpolation leicht mit dem `#{}`
  Mechanismus.
title: Zeichenketten interpolieren
weight: 8
---

## How to:
Elixir macht das Verwenden von String-Interpolation leicht mit dem `#{}` Mechanismus. Hier ein paar Beispiele:

```elixir
name = "Welt"
greeting = "Hallo, #{name}!"
IO.puts greeting
# Ausgabe: Hallo, Welt!

age = 28
message = "Ich bin #{age} Jahre alt."
IO.puts message
# Ausgabe: Ich bin 28 Jahre alt.

price = 49.99
formatted_price = "Der Preis beträgt #{price} Euro."
IO.puts formatted_price
# Ausgabe: Der Preis beträgt 49.99 Euro.
```

## Deep Dive
Elixir verwendet die String-Interpolation schon seit seiner frühen Entwicklung. Es wird von der Erlang VM unterstützt, auf der Elixir läuft, was es sehr effizient macht. Im Vergleich zu anderen Methoden zum Zusammenfügen von Strings (z.B. durch Verwendung des `++` Operators oder der `String.concat/2`-Funktion) bietet die String-Interpolation den Vorteil, dass sie lesbarer und meist performanter ist. Unter der Haube wandelt der Elixir-Compiler interpolierte Strings in effiziente Binär-Operationen um, was schnelle Ausführungszeiten gewährleistet.

Alternativen zur String-Interpolation wären:

```elixir
# Mit String.concat
IO.puts String.concat("Hallo, ", name)

# Mit dem ++ Operator
IO.puts "Hallo, " ++ name
```

Diese sind aber weniger intuitiv und können insbesondere bei der Verkettung vieler oder komplexer Ausdrücke unübersichtlich werden.

## See Also
- Erlang's String Module: [https://erlang.org/doc/man/string.html](https://erlang.org/doc/man/string.html)
- Elixir's String Module: [https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
- Elixir School String Lesson (in English): [https://elixirschool.com/en/lessons/basics/strings/](https://elixirschool.com/en/lessons/basics/strings/)
