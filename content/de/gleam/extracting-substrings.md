---
title:                "Teilstrings extrahieren"
date:                  2024-01-20T17:45:35.763418-07:00
model:                 gpt-4-1106-preview
simple_title:         "Teilstrings extrahieren"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)
Das Extrahieren von Teilzeichenketten ist das Herauslösen spezifischer Teile aus einem Textstrang. Programmierer machen das, um mit Teilinformationen zu arbeiten oder Daten zu analysieren.

## How to (Wie geht das?)
In Gleam sollst du `slice` benutzen, um Teilstrings zu extrahieren. Hier ist ein Beispiel:

```gleam
import gleam/string

pub fn main() {
  let text = "Hallo, Welt!"
  let welt = string.slice(text, 7, 11) // Startet bei 7, endet vor 11
  welt
}
```

Ausgabe:

```
"Welt"
```

## Deep Dive (Tiefer Tauchgang)
Das Herausarbeiten von Teilzeichenketten ist nichts Neues. In vielen Sprachen gibt es Funktionen wie `substring` oder `slice`, die in Gleam ihren Platz als `string.slice` gefunden haben. Gleam optimiert das Verarbeiten von Binärdaten, daher ist die Funktion effizient ohne unnötige Kopiervorgänge. Vergleichbare Alternativen in Gleam wären Rekursion oder Pattern Matching auf Strings, jedoch ist `slice` oft klarer und direkter.

## See Also (Siehe auch)
- Ein ähnliches Konzept in Elixir, einer anderen BEAM-Sprache, kannst du [hier](https://hexdocs.pm/elixir/String.html) finden.
