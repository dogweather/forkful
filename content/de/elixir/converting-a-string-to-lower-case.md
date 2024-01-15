---
title:                "Umwandeln eines Strings in Kleinbuchstaben"
html_title:           "Elixir: Umwandeln eines Strings in Kleinbuchstaben"
simple_title:         "Umwandeln eines Strings in Kleinbuchstaben"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von Strings zu Kleinbuchstaben kann nützlich sein, um Texte zu vereinheitlichen und Vergleiche oder Suchvorgänge einfacher zu gestalten. Es ist eine gängige Aufgabe in der Textverarbeitung und kann auch dazu beitragen, Fehler zu reduzieren.

## Wie geht's?

Um einen String in Elixir in Kleinbuchstaben umzuwandeln, können Sie die Funktion `String.downcase/1` verwenden.

```Elixir
string = "HALLO WELT"
String.downcase(string)
# Ausgabe: "hallo welt" 
```

Sie können auch die Pipe-Operator `|>` verwenden, um den Code leserlicher zu machen:

```Elixir
string
|> String.downcase
# Ausgabe: "hallo welt"
```

Möchten Sie alle Buchstaben eines Strings in Großbuchstaben umwandeln, können Sie `String.upcase/1` verwenden. Hier ist ein Beispiel:

```Elixir
string = "hallo welt"
String.upcase(string)
# Ausgabe: "HALLO WELT"
```

## Tiefere Einblicke

Das Konvertieren von Strings in Elixir funktioniert standardmäßig für Unicode-Zeichen, da Elixir eine erweiterte Unicode-Unterstützung bietet. Dies bedeutet, dass nicht nur Buchstaben des englischen Alphabets, sondern auch Zeichen aus anderen Sprachen korrekt konvertiert werden.

Eine weitere Funktion, die bei der Textverarbeitung hilfreich sein kann, ist `String.capitalize/1`, mit der der erste Buchstabe eines Strings in einen Großbuchstaben umgewandelt wird. Hier ist ein Beispiel für die Anwendung dieser Funktion:

```Elixir
string = "hallo welt"
String.capitalize(string)
# Ausgabe: "Hallo welt"
```

Es ist wichtig zu beachten, dass diese Funktion nur den ersten Buchstaben in einen Großbuchstaben konvertiert und nicht den Rest des Strings ändert.

## Siehe auch

- [Elixir String-Modul](https://hexdocs.pm/elixir/String.html)
- [Unicode in Elixir](https://elixir-lang.org/getting-started/unicode-charlists-and-binaries.html)