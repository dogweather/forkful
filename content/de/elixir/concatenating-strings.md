---
title:                "Zeichenketten verknüpfen"
date:                  2024-01-20T17:34:24.365843-07:00
model:                 gpt-4-1106-preview
simple_title:         "Zeichenketten verknüpfen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
String-Konkatenation ist das Verbinden von zwei oder mehreren Strings. Programmierer nutzen dies, um Textdynamisch zusammenzusetzen oder Daten zu formatieren.

## How to:
In Elixir erfolgt die Konkatenation mit dem `<>` Operator. Hier sind ein paar Beispiele:
```Elixir
string1 = "Hallo"
string2 = "Welt"
ergebnis = string1 <> " " <> string2
IO.puts(ergebnis) # Gibt aus: Hallo Welt
```

Und mit Variablen:
```Elixir
name = "Hans"
begrüßung = "Guten Tag, " <> name <> "!"
IO.puts(begrüßung) # Gibt aus: Guten Tag, Hans!
```

Auch mit Listen von Strings:
```Elixir
teile = ["Anfang-", "und", "-Ende"]
vollständig = Enum.join(teile, " ")
IO.puts(vollständig) # Gibt aus: Anfang- und -Ende
```

## Deep Dive
In Elixir sind Strings binäre Daten und UTF-8-kodiert. Historisch gesehen war die Verarbeitung von Zeichenketten in den meisten Programmiersprachen ein Kernfeature, aber die Art und Weise, wie es in Elixir gehandhabt wird, erlaubt es effizient mit Binärdaten zu arbeiten.

Alternativ zur Konkatenation gibt es auch Interpolation in Elixir, welche manchmal klarer sein kann:
```Elixir
name = "Ingrid"
nachricht = "Hallo #{name}!"
IO.puts(nachricht) # Gibt aus: Hallo Ingrid!
```

Die String-Interpolation ist oft schneller als die Konkatenation für komplexe Zusammenstellungen, weil der Erlang VM einmalig einen neuen Binärblock alloziieren kann, anstatt viele kleine zusammenzufügen.

Elixir verwendet Binärdaten unter der Haube und behandelt Unicode-Sequenzen mit voller Genauigkeit, sodass Zeichen aller Sprachen sicher verbunden werden können.

## See Also
- Offizielle Elixir Dokumentation zu Strings: https://hexdocs.pm/elixir/String.html
- Erlang's Handling von Binärdaten: https://erlang.org/doc/efficiency_guide/binaryhandling.html
- Elixir School für mehr über Strings und Interpolation: https://elixirschool.com/de/lessons/basics/strings/
