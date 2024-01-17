---
title:                "Verketten von Zeichenketten"
html_title:           "Elixir: Verketten von Zeichenketten"
simple_title:         "Verketten von Zeichenketten"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was ist das & warum?

Die Verkettung von Zeichenketten (Strings) ermöglicht es uns, mehrere Zeichenketten zu kombinieren und eine längere Zeichenkette zu erstellen. Programmierer nutzen dies, um beispielsweise Texte dynamisch zu generieren oder Dateipfade zu erstellen.

## Wie geht das?

In Elixir können wir Zeichenketten mit dem `<>` Operator zusammenfügen oder die Funktion `String.concat/2` verwenden. Beispiel:

```Elixir
"Hello" <> " " <> "World"
# Output: "Hello World"

String.concat(["Hello", " ", "World"])
# Output: "Hello World"
```

## Tiefgehende Einblicke

Die Verkettung von Zeichenketten ist ein grundlegendes Konzept der Programmierung und wird in vielen Sprachen unterstützt. In Elixir wird die Verkettung von Zeichenketten intern durch die Konkatenation von Listen implementiert. Alternativen zur Verkettung von Zeichenketten sind beispielsweise die Verwendung von Templating-Bibliotheken oder die Verwendung von regulären Ausdrücken.

## Siehe auch

- [Elixir String Dokumentation] (https://hexdocs.pm/elixir/String.html)
- [Elixir to_string() vs. <>] (https://stackoverflow.com/questions/26715380/elixir-to-string-vs)