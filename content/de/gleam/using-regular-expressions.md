---
title:                "Verwendung von regulären Ausdrücken"
html_title:           "Gleam: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke sind eine Möglichkeit, Muster in Text zu suchen und zu extrahieren. Programmierer verwenden sie, um Text zu analysieren und zu manipulieren, zum Beispiel um bestimmte Zeichenfolgen zu finden oder zu ersetzen.

## Wie geht's:
### Gleam
```Gleam import regex```

### Beispiel 1:
```Gleam
let text = "Hello, World!";
let pattern = regex.new("Hello");
let result = regex.match(pattern, text);

assert Ok("Hello") = result;
```

### Beispiel 2:
```Gleam
let text = "100 highway";
let pattern = regex.new("(\d+)");
let result = regex.all_matches(pattern, text);

assert [Ok("100")] = result;
```

## Tiefentauchen:
Reguläre Ausdrücke wurden bereits in den 1950er Jahren entwickelt, um Textverarbeitungsprogramme zu vereinfachen. Es gibt auch andere Möglichkeiten, Text zu suchen und zu extrahieren, wie z.B. String-Funktionen und Parser. Die Implementation von regulären Ausdrücken basiert auf dem sogenannten "finite-state-automaton" Ansatz, der es ermöglicht, Muster sehr effizient zu erkennen.

## Siehe Auch:
- [Gleam Dokumentation](https://gleam.run/concepts/regular-expressions.html)
- [Reguläre Ausdrücke Tutorial](https://www.regular-expressions.info/tutorial.html)
- [Python Regular Expressions](https://docs.python.org/3/howto/regex.html)