---
title:                "Elixir: Die Länge eines Strings finden"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge einer Zeichenkette mag auf den ersten Blick trivial erscheinen, aber es ist eine wichtige Fähigkeit für jeden Elixir Programmierer. Indem man die Länge einer Zeichenkette bestimmt, kann man z.B. überprüfen, ob eine Eingabe die erwartete Länge hat oder ob eine Zeichenkette länger oder kürzer als eine bestimmte Anzahl von Zeichen ist.

## Wie man die Länge einer Zeichenkette berechnet

Das Berechnen der Länge einer Zeichenkette ist einfach in Elixir. Wir können die Funktion `String.length/1` verwenden, die uns die Anzahl der Zeichen in der Zeichenkette zurückgibt. Betrachten wir dieses Beispiel:

```elixir
length = String.length("Hallo Welt!")
IO.puts "Die Länge der Zeichenkette beträgt #{length} Zeichen."
```

Dies gibt uns die Ausgabe:

`Die Länge der Zeichenkette beträgt 11 Zeichen.`

Wir können auch die `length/1` Funktion auf eine Zeichenkette anwenden, die wir aus einer anderen Funktion erhalten haben:

```elixir
name = "Maria"
greeting = "Hallo, #{name}!"
length = String.length(greeting)
```

Die Variable `length` wird den Wert `10` enthalten, da der Name "Maria" insgesamt fünf Zeichen hat und die gesamte Zeichenkette "Hallo, " zusätzlich noch fünf Zeichen hat.

## Vertiefung

Bei der Berechnung der Länge einer Zeichenkette gibt es ein paar Dinge zu beachten. Zunächst einmal gibt `String.length/1` die Anzahl der Unicode-Zeichen in einer Zeichenkette zurück und nicht die Anzahl der Bytes. Das bedeutet, dass Zeichen mit Akzenten oder anderen diakritischen Zeichen als ein einzelnes Zeichen gezählt werden, während sie in der tatsächlichen Speicherung möglicherweise aus mehreren Bytes bestehen.

Außerdem sollten wir beachten, dass bestimmte Sonderzeichen in einer Zeichenkette als zwei Unicode-Zeichen gezählt werden können. Zum Beispiel wird das Emoji "❤️" als zwei Zeichen gezählt, obwohl es nur ein Emoji ist. Dies ist wichtig zu wissen, wenn wir die Länge einer Zeichenkette mit bestimmten Einschränkungen überprüfen möchten, z.B. wenn wir eine Zeichenkette auf Twitter mit maximal 280 Zeichen posten möchten.

## Siehe auch

- Die offizielle Elixir Dokumentation für die `String.length/1` Funktion: https://hexdocs.pm/elixir/String.html#length/1 
- Elixir School: https://elixirschool.com/de/lessons/basics/strings/ 
- Eine Übersicht über Unicode-Zeichen: https://unicode-table.com/de/