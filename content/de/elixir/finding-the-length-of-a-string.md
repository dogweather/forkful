---
title:                "Elixir: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Warum

Warum sollte man sich damit beschäftigen, die Länge eines Strings zu finden? Strings sind eine grundlegende Datenstruktur in der Programmierung und es ist wichtig, ihre Länge zu kennen, um effizient mit ihnen arbeiten zu können.

# Wie

Die Länge eines Strings kann mit der `String.length` Funktion in Elixir gefunden werden. Diese Funktion akzeptiert einen String als Argument und gibt die Anzahl der Zeichen in diesem String zurück.

Beispiel:

```Elixir
text = "Hallo Welt"
String.length(text)

# Output: 10
```

# Tiefentauchen

Die `String.length` Funktion in Elixir ist effizient und läuft in konstanter Zeit, unabhängig von der Länge des Strings. Dies liegt daran, dass Strings in Elixir als UTF-8 kodiert sind und die Länge eines Strings anhand der Anzahl der Bytes bestimmt wird, anstatt der Anzahl der Zeichen.

Ein weiterer wichtiger Punkt ist, dass in Elixir Strings unveränderlich sind, dh sie können nach ihrer Erstellung nicht mehr geändert werden. Das bedeutet, dass die Länge eines Strings während der Laufzeit nicht angepasst werden kann.

# Siehe auch

- [Elixir Dokumentation über Strings](https://hexdocs.pm/elixir/String.html)
- [Tutorial zur Verwendung von Strings in Elixir](https://www.tutorialspoint.com/elixir/elixir_strings.htm)
- [Einführung in die Elixir Programmierung](https://www.freecodecamp.org/news/an-introduction-to-elixir-291a5abd0df0/)