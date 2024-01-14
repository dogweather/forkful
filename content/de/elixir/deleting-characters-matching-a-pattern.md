---
title:    "Elixir: Löschen von Zeichen, die einem Muster entsprechen"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann hilfreich sein, wenn man bestimmte Daten in einer Elixir Anwendung filtern oder formatieren möchte.

## Wie

Um Zeichen zu löschen, die einem bestimmten Muster entsprechen, kann die `String.replace/4` Funktion verwendet werden. Diese Funktion akzeptiert vier Argumente: den String, in dem das Muster gefunden werden soll, das zu ersetzende Muster, das Ersatzzeichen und einen Optionsparameter.

```Elixir
string = "Dies ist ein Beispieltext mit unerwünschten Zeichen!"

result = String.replace(string, ~r/[^a-zA-Z]/, "")

IO.puts result 

# Output: DiesisteinBeispieltextmitunerwunschtenZeichen
```

In diesem Beispiel wurde das Muster `[^a-zA-Z]` verwendet, um alle Zeichen zu löschen, die keine Buchstaben sind. Das vierte Argument `""` gibt an, dass diese Zeichen durch leere Strings ersetzt werden sollen.

## Deep Dive

Es gibt verschiedene Beispiele, bei denen das Löschen von Zeichen, die einem bestimmten Muster entsprechen, hilfreich sein kann. Zum Beispiel kann es bei der Validierung von Benutzereingaben nützlich sein, um unerwünschte Zeichen zu entfernen und nur die gültigen Zeichen zuzulassen. Es kann auch hilfreich sein, um formatierte Telefonnummern oder Postleitzahlen aus einem String zu extrahieren.

Eine weitere nützliche Funktion, um Zeichen zu löschen, die einem bestimmten Muster entsprechen, ist `Regex.replace/3`, die reguläre Ausdrücke verwendet.

```Elixir 
string = "123-456-789"

result = Regex.replace(string, ~r/[^0-9]/, "")

IO.puts result 

# Output: 123456789 
```

In diesem Beispiel wird wieder das Muster `[^0-9]` verwendet, um alle Nicht-Zahlen aus dem String zu entfernen. Diese Funktion bietet mehr Flexibilität, da sie auch komplexere Muster unterstützt.

## Siehe auch

- [Elixir String Dokumentation](https://hexdocs.pm/elixir/String.html)
- [Reguläre Ausdrücke in Elixir](https://elixirschool.com/de/lessons/advanced/regex/)
- [Musterabgleich in Elixir](https://hexdocs.pm/elixir/Kernel.SpecialForms.html#=~/2)