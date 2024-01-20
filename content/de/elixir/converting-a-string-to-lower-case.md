---
title:                "Einen String in Kleinbuchstaben umwandeln"
html_title:           "Elm: Einen String in Kleinbuchstaben umwandeln"
simple_title:         "Einen String in Kleinbuchstaben umwandeln"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Umwandeln von Zeichenketten in Kleinbuchstaben ist eine alltägliche Aufgabe in der Programmierung. Dabei wird jede Großbuchstabe in der Zeichenkette in ihren entsprechenden Kleinbuchstaben umgewandelt. Programmierer machen das oft, um den Vergleich von Zeichenketten zu erleichtern und ein einheitliches Benutzererlebnis zu gewährleisten.

## So geht's:

In Elixir sieht die Umwandlung einer Zeichenkette in Kleinbuchstaben so aus:

```elixir
iex> String.downcase("HALLO WELT")
"hallo welt"
```
Du mobilisierst einfach die `downcase` Funktion auf dem `String` Modul und gibst die Zeichenkette, die du transformieren möchtest, als Argument.

## Vertiefung

Die Behandlung von Groß- und Kleinbuchstaben in Programmiersprachen ist ein Erbe aus der Zeit der mechanischen Schreibmaschinen und hat sich bis heute in fast allen Programmiersprachen durchgesetzt. In Elixir wird die Umwandlung von Zeichenketten in Kleinbuchstaben durch die Funktion `downcase` implementiert, die auf dem Unicode-Datenbank beruht. Das bedeutet, dass sie für alle Zeichenketten arbeitet, unabhängig von der Sprache oder dem Alphabet, das sie verwenden. Es gibt Alternativen zur `downcase` Funktion, wie die `swapcase` Funktion, die jede Kleinbuchstabe in ihre entsprechende Großbuchstabe umwandelt, und umgekehrt.

## Siehe auch

Weitere Informationen finden Sie unter:

- Elixir Offiziell: String Modul (https://hexdocs.pm/elixir/String.html)
- Erlang/OTP Quellcode von Elixir (https://github.com/elixir-lang/elixir)
- Elixir Forum (https://elixirforum.com/)