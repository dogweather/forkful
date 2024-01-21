---
title:                "Suchen und Ersetzen von Text"
date:                  2024-01-20T17:57:43.220468-07:00
model:                 gpt-4-1106-preview
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Was & Warum?
Suchen und Ersetzen von Text ist das Auffinden spezifischer Zeichenmuster in einer Zeichenkette und deren Austausch durch andere Inhalte. Programmierer nutzen diese Funktion, um Daten zu bereinigen, Code zu refaktorisieren oder massenhafte Änderungen effizient durchzuführen.

## So geht's:
In Gleam, um Text zu suchen und zu ersetzen, verwenden wir reguläre Ausdrücke oder String-Funktionen. Hier ein einfaches Beispiel:

```gleam
import gleam/regex

pub fn search_and_replace(text: String) -> String {
  let pattern = regex.regex("Welt").unwrap()
  regex.replace(pattern, text, "Gleam")
}

fn main() {
  let text = "Hallo Welt!"
  let new_text = search_and_replace(text)
  new_text
}
```

Beispielausgabe:

```
"Hallo Gleam!"
```

## Tiefgang:
Früher in der Programmierung, insbesondere vor der breiten Verfügbarkeit leistungsfähiger Bibliotheken, war das Suchen und Ersetzen von Text eine mühsame Aufgabe, die oft in maschinennahem Code implementiert wurde. Heute bieten viele Sprachen, wie Gleam, eingebaute Funktionen, die mit Mustervergleichslogik, sogenannten regulären Ausdrücken, arbeiten. Alternativen zum Suchen und Ersetzen mit regulären Ausdrücken sind String-Funktionen, die direkt spezifische Zeichen suchen und ohne Pattern-Matching auskommen. Im Hintergrund kann die Implementierung von Suchen und Ersetzen mithilfe von regulären Ausdrücken komplex sein, denn sie nutzt Algorithmen wie den Knuth-Morris-Pratt-Algorithmus oder den Boyer-Moore-Algorithmus für effiziente Textverarbeitung.

## Siehe auch:
- Eine Einführung in reguläre Ausdrücke: [https://www.regular-expressions.info/](https://www.regular-expressions.info/)
- Textverarbeitungsalgorithmen: [https://en.wikipedia.org/wiki/String-searching_algorithm](https://en.wikipedia.org/wiki/String-searching_algorithm)