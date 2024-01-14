---
title:                "Elixir: Ausgabe von Debug-Meldungen"
simple_title:         "Ausgabe von Debug-Meldungen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Debug-Ausgaben sind eine nützliche Methode, um bei der Entwicklung von Elixir-Programmen Fehler zu finden und Probleme zu beheben. Mit dem richtigen Ansatz können sie den Entwicklungsprozess beschleunigen und die Effizienz steigern.

## How To

Um Debug-Ausgaben in Elixir zu erstellen, können wir die Funktion `IO.inspect/2` verwenden. Diese Funktion nimmt zwei Argumente entgegen: den Wert, der ausgegeben werden soll, und eine optionale Liste von Optionen.

Ein einfaches Beispiel wäre wie folgt:

```Elixir
IO.inspect("Hallo Welt")
```

Dies würde "Hallo Welt" auf der Konsole ausgeben. Wir können auch die `:label`-Option verwenden, um die Ausgabe zu kennzeichnen:

```Elixir
IO.inspect("Hallo Welt", label: "Grüße:")
```

Dies würde die Ausgabe als "Grüße: " Hallo Welt" ausgeben. Ein weiteres nützliches Argument ist die `:depth`-Option, mit der wir die Tiefe der Ausgabe angeben können, um komplexe Datenstrukturen besser zu verstehen.

Wir können auch `IO.inspect` in Funktionen verwenden, um den Wert von Variablen zu überwachen und zu debuggen:

```Elixir
def add_numbers(a, b) do
  result = a + b
  IO.inspect(result, label: "Ergebnis:")
  result
end
```

In diesem Beispiel würden wir das Ergebnis jeder Addition ausgeben, um sicherzustellen, dass unsere Funktion korrekt funktioniert.

## Deep Dive

Die `IO.inspect`-Funktion kann auch als `require` in Modulen aufgerufen werden, um Debug-Ausgaben zu erstellen. Wir können dies tun, indem wir `require` im Modul platzieren und das `:compile_env`-Option verwenden, um zu steuern, ob `IO.inspect`-Aufrufe im Produktionsmodus entfernt werden sollen.

Zusätzlich zur `IO.inspect`-Funktion gibt es auch eine `Logger`-Bibliothek, die erweiterte Funktionen zum Protokollieren und Debuggen von Ausgaben bietet. Diese lässt sich in verschiedenen Elixir-Projekten nutzen und bietet zusätzliche Optionen wie Level-Filterung und die Möglichkeit, Ausgaben in Dateien zu speichern.

## Siehe auch

- Dokumentation zu `IO.inspect` [(Link)](https://hexdocs.pm/elixir/IO.html#inspect/2)
- Elixir Logger-Dokumentation [(Link)](https://hexdocs.pm/logger/Logger.html)
- Einführung in Elixir-Debugging-Tools [(Link)](https://blog.appsignal.com/2019/11/05/debugging-elixir.html)