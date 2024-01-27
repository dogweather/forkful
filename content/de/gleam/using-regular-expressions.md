---
title:                "Einsatz von regulären Ausdrücken"
date:                  2024-01-19
html_title:           "Bash: Einsatz von regulären Ausdrücken"
simple_title:         "Einsatz von regulären Ausdrücken"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke, kurz Regex, sind Muster, mit denen wir Text nach bestimmten Regeln durchsuchen und manipulieren können. Programmierer nutzen sie, um z.B. Eingaben zu validieren, Text zu ersetzen oder Daten zu extrahieren – effizient und flexibel.

## How to:
Ein Gleam-Beispiel zeigt, wie wir Regex verwenden, um eine E-Mail-Adresse zu prüfen:

```gleam
import gleam/regex

pub fn main() {
  let re = regex.regex("^[\\w.-]+@[\\w.-]+\\.\\w+$").unwrap()
  let is_valid_email = regex.find(re, "name@example.com").is_some()

  is_valid_email
  // Output: true
}
```

Und zum Extrahieren von Zahlen aus einem String:

```gleam
pub fn extract_numbers(text: String) -> List(String) {
  let re = regex.regex("\\d+").unwrap()
  regex.find_all(re, text)
    |> List.map(fn(x) { x.match_ })
}

pub fn main() {
  extract_numbers("Nummern 123 und 456").to_string()
  // Output: "[123, 456]"
}
```

## Deep Dive:
Regex entstand in den 1950ern und wurde in den 70ern durch die Unix-Welt populär. Alternativen zu Regex sind String-spezifische Funktionen wie `contains` oder Parsing-Bibliotheken, die für bestimmte Datenformate (z.B. JSON) optimiert sind. Intern verwendet Regex oft einen Zustandsautomaten, der die Muster effizient prüft.

## See Also:
- Online Regex-Tester, um Muster zu testen: [https://regex101.com/](https://regex101.com/)
- "Mastering Regular Expressions" von Jeffrey E.F. Friedl für ein tiefes Verständnis der Materie.
