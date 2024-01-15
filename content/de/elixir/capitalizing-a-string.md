---
title:                "String in Großbuchstaben umwandeln"
html_title:           "Elixir: String in Großbuchstaben umwandeln"
simple_title:         "String in Großbuchstaben umwandeln"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Obwohl es auf den ersten Blick wie eine einfache Aufgabe erscheint, kann das Kapitalisieren von Strings in Elixir in manchen Fällen eine wichtige Rolle spielen. Zum Beispiel kann es hilfreich sein, wenn Benutzereingaben in einem Formular standardisiert werden sollen, um Datenkonsistenz zu gewährleisten oder wenn Text für die Ausgabe formatiert werden soll.

## Wie man es macht

Um einen String in Elixir zu kapitalisieren, kannst du die Funktion `String.capitalize/1` verwenden. Hier ist ein Beispielcode:

```elixir
name = "max mustermann"
kapitalisierter_name = String.capitalize(name)
IO.puts(kapitalisierter_name)
```
Dies würde die folgende Ausgabe erzeugen:

```elixir
Max Mustermann
```

Du kannst auch `String.capitalize/2` verwenden, um eine benutzerdefinierte Trennung zwischen Wörtern anzugeben. Wenn du zum Beispiel möchtest, dass der String "max mustermann" als "Max-Mustermann" formatiert wird, könntest du folgenden Code verwenden:

```elixir
name = "max mustermann"
kapitalisierter_name = String.capitalize(name, "-")
IO.puts(kapitalisierter_name)
```

Dies würde die folgende Ausgabe erzeugen:

```elixir
Max-Mustermann
```

## Tiefer gehende Informationen

Wenn du dein Wissen über die Funktion `String.capitalize/1` auf die Probe stellen möchtest, gibt es ein paar Dinge, die du beachten solltest. Zum Beispiel wird die Funktion die Groß- und Kleinschreibung von Akzenten und diakritischen Zeichen nicht verändern. Außerdem wird sie nur den ersten Buchstaben jedes Wortes in einem String groß schreiben, nicht jedoch beispielsweise alle Buchstaben nach einem Bindestrich.

Um diese Funktionalitäten zu erreichen, gibt es andere Funktionen wie `String.upcase/1`, `String.downcase/1` oder `String.trim_leading/2`, die in bestimmten Fällen besser geeignet sein könnten.

## Siehe auch

- [String.capitalize/1 Dokumentation](https://hexdocs.pm/elixir/String.html#capitalize/1)
- [Offizielle Elixir Webseite](https://elixir-lang.org/)
- [Elixir Forum](https://elixirforum.com/)