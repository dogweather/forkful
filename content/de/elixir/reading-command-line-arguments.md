---
title:                "Das Lesen von Befehlszeilenargumenten"
html_title:           "Elixir: Das Lesen von Befehlszeilenargumenten"
simple_title:         "Das Lesen von Befehlszeilenargumenten"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Command Line Arguments, oder auf Deutsch Befehlszeilenargumente, sind eine wichtige Funktion in der Programmierung. Sie erlauben es uns, beim Starten einer Anwendung spezifische Parameter mitzugeben, die dann von unserem Programm ausgewertet werden können. Das kann sehr nützlich sein, um das Verhalten unseres Codes anzupassen und ihn flexibler zu machen.

## How To

Um in Elixir Befehlszeilenargumente zu lesen, können wir auf die `System.argv` Funktion zurückgreifen. Diese gibt uns eine Liste mit den übergebenen Argumenten zurück. Im folgenden Beispiel lesen wir zwei Argumente aus und geben sie anschließend aus:

```Elixir
args = System.argv
IO.puts("Erstes Argument: #{Enum.at(args, 0)}")
IO.puts("Zweites Argument: #{Enum.at(args, 1)}")
```

Mit `Enum.at` können wir auf bestimmte Indizes der Liste zugreifen und die Argumente so auslesen. Wenn keine Argumente angegeben wurden, wird die Liste einfach leer sein.

## Deep Dive

Wenn wir einen genaueren Blick auf die `System.argv` Funktion werfen, sehen wir, dass sie eine Option `:raw` hat. Diese gibt uns eine Liste mit den übergebenen Argumenten zurück, ohne sie vorher zu interpretieren. Das kann in manchen Fällen nützlich sein, wenn wir die Argumente selbst auswerten möchten. Außerdem können wir mit `System.argv(Enum, :argc)` die Anzahl der übergebenen Argumente auslesen.

See Also

- Offizielle Dokumentation: https://hexdocs.pm/elixir/System.html#argv/1
- "Command Line Arguments in Elixir" von Philipp Krüger: https://dev.to/philippk/elixir-command-line-arguments-4ma5