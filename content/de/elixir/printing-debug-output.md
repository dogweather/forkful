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

## Was & Warum?
Das Drucken von Debug-Ausgaben ist ein Prozess, bei dem Programmierer zusätzlichen Code in ihre Anwendung einfügen, um bestimmte Variablen oder Funktionen während der Ausführung anzuzeigen. Dies kann hilfreich sein, um Fehler oder Probleme in einem Programm zu identifizieren und zu beheben.

## Wie geht's:
Um Debug-Ausgaben in Elixir zu drucken, verwenden wir die Funktion `IO.inspect`. Dies erlaubt uns, eine beliebige Variable oder Funktion als Argument zu übergeben, die dann ausgegeben wird. Zum Beispiel:

```Elixir
a = 5
IO.inspect(a)
```

Dieser Code würde die Zahl 5 in der Konsole ausgeben. Wir können auch mehrere Argumente an `IO.inspect` übergeben, indem wir sie einfach mit Kommas trennen:

```Elixir
IO.inspect(a, "Der Wert von A ist:")
```

Dies würde die Nachricht "Der Wert von A ist: 5" ausgeben.

## Tiefentauchen:
Das Drucken von Debug-Ausgaben ist eine gemeinsame Praxis, die von Programmierern verwendet wird, um Probleme in ihrem Code zu diagnostizieren. Es ist oft eine einfachere und effektivere Methode als das klassische Debugging mit Breakpoints. Alternativ können Programmierer auch spezielle Debugging-Tools wie den Visual Studio Code Debugger oder den Elixir Debugging Tracer verwenden. Die `IO.inspect` Funktion ist ein eingebauter Teil der Elixir-Sprache, was bedeutet, dass sie immer verfügbar ist und keine zusätzlichen Abhängigkeiten erfordert.

## Siehe auch:
Offizielle Elixir Dokumentation zu `IO.inspect`: https://hexdocs.pm/elixir/IO.html#inspect/2
Ein Tutorial zur Verwendung von `IO.inspect`: https://elixirschool.com/lessons/advanced/io-inspect/