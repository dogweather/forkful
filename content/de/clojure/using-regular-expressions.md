---
title:                "Einsatz von regulären Ausdrücken"
html_title:           "Bash: Einsatz von regulären Ausdrücken"
simple_title:         "Einsatz von regulären Ausdrücken"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke sind Muster, die zum Suchen und Ersetzen von Text dienen. Programmierer verwenden sie, um Daten zu analysieren und zu manipulieren, da sie leistungsstark und flexibel sind.

## How to:
```Clojure
;; Matching a pattern
(re-find #"\bClojure\b" "I love programming in Clojure!")
; => "Clojure"

;; Splitting a string at each whitespace
(re-seq #"\s+" "Split this string at spaces")
; => (" " " " " " " ")

;; Replacing all occurrences of a pattern
(clojure.string/replace "Replace dashes-with spaces" #"-+" " ")
; => "Replace dashes with spaces"

;; Extracting all words from a string
(re-seq #"\w+" "Find all words in 2023!")
; => ("Find" "all" "words" "in" "2023")
```

## Deep Dive
Reguläre Ausdrücke entstanden in den 1950ern und wurden durch Tools wie grep in Unix populär. Alternativen wie String-Manipulationsfunktionen oder Parsers bieten oft mehr Lesbarkeit und Wartbarkeit. Clojure's `re-find`, `re-seq`, und Funktionen des `clojure.string` Namespaces implementieren reguläre Ausdrücke effizient, indem sie auf Java's `java.util.regex` API aufbauen.

## See Also
- ClojureDocs zu regulären Ausdrücken: https://clojuredocs.org/clojure.core/re-find
- Java Pattern Klasse Dokumentation: https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html
- Online Regex Tester und Debugger: https://regex101.com/
