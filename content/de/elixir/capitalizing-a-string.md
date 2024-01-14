---
title:                "Elixir: Großschreibung eines Strings"
simple_title:         "Großschreibung eines Strings"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Capitalisieren einer Zeichenkette ist eine nützliche Funktion, um Strings in einer Elixir-Anwendung einheitlich zu formatieren. Dies kann hilfreich sein, um zum Beispiel Benutzernamen oder Titel in einer korrekten Schreibweise anzuzeigen oder um Daten zu filtern.

## Wie man es macht

Die einfachste Möglichkeit, eine Zeichenkette in Elixir zu capitalisieren, ist die Verwendung der `String.capitalize/1` Funktion. Diese Funktion akzeptiert einen String als Argument und gibt den String zurück, wobei der erste Buchstabe großgeschrieben wird:

```Elixir
String.capitalize("hallo") # => "Hallo"
```

Man kann die Funktion auch auf eine Liste von Wörtern anwenden, wobei jeder erste Buchstabe in den einzelnen Wörtern großgeschrieben wird:

```Elixir
String.capitalize("guten tag") # => "Guten Tag"
```

Für Zeichenketten, die bereits großgeschrieben sind, bleibt die Funktion unverändert:

```Elixir
String.capitalize("ELIXIR") # => "ELIXIR"
```

Es gibt auch andere Funktionen wie `String.capitalize/2` und `String.capitalize_every/2`, die mehr Kontrolle über die Capitalisierung bieten. Es ist empfehlenswert, die Dokumentation zu diesen Funktionen zu lesen, um die beste Lösung für spezifische Anwendungsfälle zu finden.

## Tiefergehende Informationen

Die `String.capitalize/1` Funktion verwendet unter der Haube die `String.upcase/1` Funktion, um den ersten Buchstaben zu capitalisieren. Diese Funktion verwendet die Unicode-Kategorie von Buchstaben, um zu entscheiden, welches Zeichen großgeschrieben werden soll. Dies bedeutet, dass sie auch mit Nicht-Alphabet-Zeichen wie Akzenten oder Umlauten umgehen kann.

Es ist auch wichtig zu beachten, dass die Capitalisierung von Zeichenketten in Elixir nicht nur auf Englisch beschränkt ist. Durch die Verwendung von Unicode-Zeichen können Zeichenketten aus verschiedenen Sprachen auch auf korrekte Weise capitalisiert werden.

## Siehe auch

- [Elixir String-Dokumentation](https://hexdocs.pm/elixir/String.html)
- [Unicode-Kategorien](https://www.unicode.org/reports/tr44/)
- [Elixir-Typkonvertierung](https://elixirschool.com/de/lessons/basics/type-conversion/)