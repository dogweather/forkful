---
title:                "String in Großbuchstaben umwandeln"
date:                  2024-01-19
html_title:           "C: String in Großbuchstaben umwandeln"
simple_title:         "String in Großbuchstaben umwandeln"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Großschreibung eines Strings bedeutet, alle Buchstaben in einem String in Großbuchstaben umzuwandeln. Programmierer nutzen diese Methode, um Konsistenz zu gewährleisten, beispielsweise bei Nutzereingaben oder um Text hervorzuheben.

## So geht's:
Clojure bietet verschiedene Wege, einen String großzuschreiben. Hier ein paar Beispiele:

```clojure
;; Einfacher Weg, um einen String komplett großzuschreiben
(.toUpperCase "Ich bin ein kleiner Satz.")
;; Ausgabe: "ICH BIN EIN KLEINER SATZ."

;; Benutzung der `clojure.string` Bibliothek
(require '[clojure.string :as str])
(str/upper-case "Fuchs springt über den faulen Hund.")
;; Ausgabe: "FUCHS SPRINGT ÜBER DEN FAULEN HUND."
```

## Tiefgang:
Ursprünglich kommt die Praxis, Text großzuschreiben, aus der Zeit, als ausschließlich Großbuchstaben genutzt wurden – denken wir an römische Inschriften. In Clojure, wie in vielen modernen Sprachen, gibt es mehrere Wege, Strings zu manipulieren, wobei `.toUpperCase` und `clojure.string/upper-case` am gebräuchlichsten sind.

Beim Umgang mit internationalen Strings ist Vorsicht geboten: bestimmte Sprachen haben spezielle Großschreibregeln. Clojure folgt Java's Methode, um Großschreibung Unicode-kompatibel zu machen, aber es ist wichtig, die kulturellen Besonderheiten zu beachten.

`.toUpperCase` ist eine Methode, die auf Java's `String` Klasse operiert, während `clojure.string/upper-case` Teil von Clojures eigener string manipulation library ist, die auf Clojure's Datenstrukturen zugeschnitten ist. Beide liefern ähnliche Ergebnisse, doch die Wahl hängt oft davon ab, ob man innerhalb einer "Clojure-nativen" Umgebung bleiben oder die Interoperabilität mit Java nutzen möchte.

## Siehe Auch:
- ClojureDocs zu `clojure.string`: https://clojuredocs.org/clojure.string
- Ein Guide zu Clojure Strings: https://www.clojure-toolbox.com/
- Unicode Groß- und Kleinschreibung: https://unicode.org/reports/tr21/
- Java String Dokumentation: https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html
