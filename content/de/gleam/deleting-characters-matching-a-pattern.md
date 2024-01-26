---
title:                "Löschen von Zeichen, die einem Muster entsprechen"
date:                  2024-01-20T17:42:16.694990-07:00
model:                 gpt-4-1106-preview
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)
Das Löschen von Zeichen, die einem Muster entsprechen, bedeutet, bestimmte Zeichen aus einem String zu entfernen. Programmierer tun dies, um Daten zu bereinigen, Nutzereingaben zu verarbeiten oder Strings für bestimmte Zwecke zuzuschneiden.

## How to: (Wie geht das:)
```gleam
import gleam/regex.{replace}

pub fn remove_pattern(text: String) -> String {
  replace(text, regex.from_string("[aeiou]").unwrap(), "")
}

fn main() {
  let clean_text = remove_pattern("Hier steht ein Beispieltext.")
  io.println(clean_text) // Output: "Hr stht n Bspltxt."
}
```

## Deep Dive (Tieftauchgang)
Historisch gesehen stammt die Manipulation von Zeichenketten aus der Notwendigkeit, Texte für verschiedene Computeranwendungen zu formatieren. Gleam nutzt reguläre Ausdrücke ähnlich wie viele andere Programmiersprachen, um mit Mustern in textbasierten Daten zu arbeiten. Alternativen zu regulären Ausdrücken können String-Methoden wie `slice`, `split` oder `trim` sein, die schneller, aber weniger flexibel sind. Bei der Verwendung von `regex.from_string`, ist es wichtig `.unwrap()` verantwortungsvoll einzusetzen, da es bei Fehlerhaften Patterns zu Laufzeitfehlern führen kann.

## See Also (Siehe auch)
- Regular expressions in programming: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- String manipulation techniques: [https://docs.python.org/3/library/stdtypes.html#string-methods](https://docs.python.org/3/library/stdtypes.html#string-methods)
