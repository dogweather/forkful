---
title:                "Elixir: Großschreibung eines Strings"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Warum

Es gibt viele Gründe, warum Sie eine Zeichenfolge in Elixir großschreiben möchten. Möglicherweise möchten Sie die Benutzerfreundlichkeit Ihrer Anwendung verbessern, indem Sie sicherstellen, dass Texte konsistent dargestellt werden, oder Sie benötigen eine bestimmte Formatierung für die Ausgabe von Daten.

# Wie geht's

Die Capitalize-Funktion in Elixir ist sehr einfach zu verwenden. Sie nehmen einfach die Zeichenfolge, die Sie großschreiben möchten, und verwenden die Funktion `String.capitalize/1`. Hier ist ein Beispiel:

```Elixir
name = "peter"
capitalized_name = String.capitalize(name)
IO.puts(capitalized_name)
```

Das oben genannte Beispiel würde die Ausgabe `Peter` zurückgeben. Wenn Sie möchten, können Sie auch nur den ersten Buchstaben einer Zeichenfolge großschreiben, indem Sie die Funktion `String.capitalize/1` verwenden. Hier ist ein Beispiel:

```Elixir
phrase = "elixir programming"
capitalized_phrase = String.capitalize(phrase)
IO.puts(capitalized_phrase)
```

Die Ausgabe wäre `Elixir programming`.

# Tiefer Einblick

Die `String.capitalize/1` Funktion funktioniert, indem sie den ersten Buchstaben der Zeichenfolge in einen Großbuchstaben umwandelt und alle anderen Buchstaben in Kleinschreibung umwandelt. Wenn Sie jedoch eine Zeichenfolge haben, die bereits großgeschrieben ist, wird die Funktion keine Änderungen vornehmen.

Eine wichtige Sache zu beachten ist, dass diese Funktion Sprach-spezifisch ist. Dies bedeutet, dass die Ausgabe von `String.capitalize/1` auf der aktuellen Elixir-Konfiguration basiert. Wenn Sie also eine nicht-englische Spracheinstellung haben, wird die Ausgabe möglicherweise anders sein.

# Siehe auch

- [String.capitalize/1 Dokumentation](https://hexdocs.pm/elixir/String.html#capitalize/1)
- [Offizielle Elixir Website](https://elixir-lang.org/)
- [Elixir Forum](https://elixirforum.com/)