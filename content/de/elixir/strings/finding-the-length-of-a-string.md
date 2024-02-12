---
title:                "Ermittlung der Zeichenkettenlänge"
aliases:
- /de/elixir/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:07.697168-07:00
model:                 gpt-4-1106-preview
simple_title:         "Ermittlung der Zeichenkettenlänge"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Länge eines Strings zu ermitteln bedeutet, zu zählen, wie viele Zeichen er enthält. Diese Information ist wichtig für Input-Validierung, Textverarbeitung oder wenn die Leistung von Algorithmen auf Zeichenkettenlänge basiert.

## So geht’s:
In Elixir kannst du einfach die eingebaute Funktion `String.length/1` verwenden:

```elixir
string = "Hallo Welt!"
string_length = String.length(string)
IO.puts string_length
```

Ausgabe:

```
11
```

## Tiefgang:
Historisch gesehen ist die Aufgabe, die Länge eines Strings zu finden, so alt wie das Programmieren mit Zeichenketten selbst. Elixir behandelt Strings als Binärdaten in UTF-8-Codierung, was bedeutet, dass die Längesfunktion die Anzahl der Unicode-Grapheme und nicht die Anzahl der Bytes zurückgibt. Alternativ könntest du die `byte_size/1` Funktion verwenden, um die Anzahl der Bytes statt der Zeichen zu bekommen, was bei der Arbeit mit binären Daten nützlich sein kann. Die `String.length/1` Funktion ist jedoch die passendste Wahl für die meisten Textaufgaben, da sie genau auf die Bedürfnisse der String-Verarbeitung zugeschnitten ist.

## Siehe Auch:
- Elixir Dokumentation für Strings: [Elixir String Docs](https://hexdocs.pm/elixir/String.html)
- Elixir Forum für Fragen und Diskussionen: [Elixir Forum](https://elixirforum.com)
