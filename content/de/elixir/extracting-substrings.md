---
title:                "Elixir: Extrahieren von Teilstrings"
simple_title:         "Extrahieren von Teilstrings"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Das Extrahieren von Teilzeichenketten ist ein nützliches Werkzeug für Entwickler, um bestimmte Teile von Texten oder Zeichenketten zu isolieren. Dies kann besonders hilfreich sein, wenn man mit großen Datensätzen arbeitet oder wenn man spezifische Informationen aus einem Text extrahieren möchte.

## Wie man Teilzeichenketten extrahiert

Um Teilzeichenketten in Elixir zu extrahieren, gibt es verschiedene Möglichkeiten. Eine davon ist die Verwendung der `String.slice/3` Funktion. Diese Funktion nimmt eine Zeichenkette, den Startindex und die Länge als Argumente und gibt eine Teilzeichenkette zurück, die am angegebenen Startindex beginnt und eine bestimmte Länge hat.

```
Elixir> String.slice("Dies ist ein Beispiel", 5, 4)
s ist
```

Eine andere Möglichkeit ist die Verwendung der `String.split/3` Funktion, die eine Zeichenkette anhand eines Trennzeichens in eine Liste von Teilzeichenketten aufteilt.

```
Elixir> String.split("Apfel,Birne,Orange", ",")
["Apfel", "Birne", "Orange"]
```

Zusätzlich gibt es auch die `Regex.split/2` Funktion, die eine Zeichenkette anhand eines regulären Ausdrucks in eine Liste von Teilzeichenketten aufteilt.

```
Elixir> Regex.split("Dies ist ein Beispiel", ~r/[aeiou]/)
["D", "s ", "st ", "n B", "sp", "l"]
```

All diese Funktionen können kombiniert werden, um komplexe Extraktionen durchzuführen und verschiedene Teile einer Zeichenkette zu isolieren.

## Tiefergehender Einblick

Es gibt noch viele weitere Funktionen und Methoden in Elixir, die beim Extrahieren von Teilzeichenketten nützlich sein können. Dazu gehören zum Beispiel `String.replace/4`, `String.trim/2` oder auch die Verwendung von regulären Ausdrücken mit der `Regex` Module.

Es ist auch wichtig, zu beachten, dass Elixir Zeichenketten als Binärdaten behandelt und daher einige Funktionen wie `String.slice/3` bei der Arbeit mit speziellen Zeichen wie Emojis oder nicht-lateinischen Zeichen möglicherweise nicht wie erwartet funktionieren. In solchen Fällen können die Funktionen im `Unicode` Modul eine bessere Alternative bieten.

## Siehe auch

- [Offizielle Elixir Dokumentation](https://elixir-lang.org/getting-started/string-patterns-and-regular-expressions.html#string-slicing)
- [Elixir Cookbook: String Manipulation](https://elixircasts.io/elixir-cookbook/string-manipulation)
- [Elixir School: Strings](https://elixirschool.com/en/lessons/basics/strings/)