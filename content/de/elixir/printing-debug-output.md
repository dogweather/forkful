---
title:                "Debug-Ausgabe drucken"
html_title:           "Elixir: Debug-Ausgabe drucken"
simple_title:         "Debug-Ausgabe drucken"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Debug-Ausgaben sind ein wichtiges Werkzeug für Programmierer, um Fehler in ihrem Code zu finden und zu beheben. Sie ermöglichen es uns, den genauen Ablauf einer Funktion oder Methode zu verfolgen und zu überprüfen, ob die erwarteten Werte zurückgegeben werden. Durch das Drucken von Debug-Ausgaben können wir effizienter und schneller Probleme in unserem Code lösen.

## Wie geht's

Um Debug-Ausgaben in Elixir zu drucken, verwenden wir die Funktion `IO.inspect`. Diese Funktion nimmt einen beliebigen Ausdruck als Argument und gibt ihn in der Konsole aus. Wir können `IO.inspect` in unsere Funktionen oder Methoden einfügen, um zu überprüfen, welche Werte an bestimmten Stellen im Code vorhanden sind.

Hier ist ein Beispiel, wie wir `IO.inspect` verwenden können:

```elixir
defp add_numbers(a, b) do
  result = a + b
  IO.inspect("The result is: #{result}")
  result
end

add_numbers(2, 3)
```

Der obige Code gibt `The result is: 5` in der Konsole aus. Auf diese Weise können wir überprüfen, ob unsere Funktion das erwartete Ergebnis zurückgibt.

Eine andere nützliche Funktion für Debugging-Zwecke ist `IO.puts`. Diese Funktion gibt einen String in der Konsole aus und fügt automatisch eine Zeilenumbruch am Ende hinzu. Wir können `IO.puts` ebenfalls verwenden, um informative Nachrichten in unseren Ausgaben hinzuzufügen.

## Tiefer tauchen

Es gibt noch ein paar weitere Funktionen, die wir für Debug-Ausgaben in Elixir verwenden können, wie zum Beispiel `IO.inspect` mit der Option `:label`, um unsere Ausgaben zu beschriften, oder `IO.inspect` mit der Option `:skip_io`, um das Drucken auf der Konsole zu überspringen und stattdessen den Wert zurückzugeben.

Es ist auch möglich, ausgefeiltere Debugging-Techniken in Elixir anzuwenden, wie z.B. die Verwendung von Libs wie [IEx](https://hexdocs.pm/iex/IEx.html), die es uns ermöglicht, in einer interaktiven Umgebung zu debuggen.

## Siehe auch

- [Elixir Dokumentation - IO.inspect](https://hexdocs.pm/elixir/IO.html#inspect/2)
- [Elixir Dokumentation - IO.puts](https://hexdocs.pm/elixir/IO.html#puts/1)
- [IEx Dokumentation](https://hexdocs.pm/iex/overview.html)