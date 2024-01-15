---
title:                "Löschen von Zeichen, die einem Muster entsprechen"
html_title:           "Elixir: Löschen von Zeichen, die einem Muster entsprechen"
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann in verschiedenen Situationen nützlich sein. Beispielsweise kann es helfen, unerwünschte Zeichen aus einer Zeichenfolge zu entfernen oder die Formatierung von Daten zu bereinigen. 

## Wie geht das?

Um in Elixir Zeichen zu löschen, die einem bestimmten Muster entsprechen, können wir die `String.replace/4`-Funktion verwenden. Diese Funktion akzeptiert ein Muster, eine Substitution und eine Zeichenfolge als Argumente. Hier ist ein Beispiel, das alle Leerzeichen aus einer Zeichenfolge entfernt:

```elixir
iex> String.replace("Dies ist ein Beispiel", ~r/\s+/, "")
"DiesisteinBeispiel"
```

Die `String.replace/4`-Funktion gibt eine neue Zeichenfolge zurück, in der das angegebene Muster durch die Substitution ersetzt wurde. Dies ermöglicht es uns, unerwünschte Zeichen einfach zu entfernen.

## Tiefentauchen

Um besser zu verstehen, wie die `String.replace/4`-Funktion arbeitet, können wir uns ansehen, wie reguläre Ausdrücke in Elixir funktionieren. Reguläre Ausdrücke sind ein mächtiges Konzept, das es uns ermöglicht, Zeichenfolgen basierend auf einem bestimmten Muster zu manipulieren.

In Elixir können reguläre Ausdrücke entweder direkt als regulärer Ausdruck oder als regulärer Ausdruck zusammen mit Flaggen angegeben werden. Hier ist ein Beispiel, das alle Buchstaben in einer Zeichenfolge in Großbuchstaben umwandelt:

```elixir
iex> String.replace("Hallo Welt", ~r/\w+/, &String.upcase/1)
"HALLO WELT"
```

In diesem Beispiel verwenden wir die `&String.upcase/1`-Funktion als Substitution, um jeden gefundenen Buchstaben in Großbuchstaben umzuwandeln. Dies zeigt die Flexibilität von regulären Ausdrücken und wie sie in Kombination mit der `String.replace/4`-Funktion verwendet werden können.

## Siehe auch

- [Elixir Dokumentation zu regulären Ausdrücken](https://hexdocs.pm/elixir/Regex.html)
- [Weitere Beispiele zur Verwendung von regulären Ausdrücken in Elixir](https://www.codedrome.com/regular-expressions-in-elixir/)