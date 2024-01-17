---
title:                "Verwendung von regulären Ausdrücken"
html_title:           "Clojure: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

Was sind reguläre Ausdrücke und warum sind sie für Programmierer wichtig?

Reguläre Ausdrücke sind spezielle Zeichenfolgen, die verwendet werden, um bestimmte Muster in Texten zu suchen und zu manipulieren. Programmierer nutzen reguläre Ausdrücke, um komplexe Suchanfragen zu vereinfachen und effizienter zu gestalten. Sie sind besonders nützlich bei der Arbeit mit Textdaten, z.B. beim Validieren von Formulardaten oder Extrahieren von Informationen aus großen Textmengen.

So nutzt du reguläre Ausdrücke in Clojure:

```Clojure
; Zuerst importiere das "regex" Modul
(ns regex-example
  (:require [clojure.string :as str]))

; Suche nach einer bestimmten Zeichenfolge in einem Text
(str/find #"Hallo" "Hallo Welt") ; Ergebnis: #{"Hallo"}

; Suche nach mehreren möglichen Zeichenfolgen
(str/find #"H(e|a)y" "Hey there") ; Ergebnis: #{"Hey"}

; Ersetze eine Zeichenfolge mit einer anderen
(str/replace "abc123" #"123" "xyz") ; Ergebnis: "abcxyz"

; Nutze Gruppen, um Teile des gefundenen Musters zu extrahieren
(str/re-matches #"(\d+)-(\w+)" "123-abc") ; Ergebnis: ["123-abc" "123" "abc"]
```

Grundlagen geschafft? Gut, dann lies weiter für einen tieferen Einblick in reguläre Ausdrücke.

Ein kurzer Ausflug in die Geschichte

Reguläre Ausdrücke wurden in den 1950er Jahren von dem Mathematiker Stephen Kleene entwickelt. Sie fanden zunächst Anwendung in der formalen Sprachtheorie und wurden später auch in Programmiersprachen implementiert. Heute sind sie aus der Welt der Textverarbeitung nicht mehr wegzudenken.

Alternativen zu regulären Ausdrücken

In Clojure gibt es verschiedene andere Funktionen, die ähnliche Aufgaben erfüllen können, wie z.B. die Funktion `string/contains?` zum Überprüfen, ob eine Zeichenfolge eine andere enthält. Allerdings sind diese Funktionen nicht so flexibel und leistungsstark wie reguläre Ausdrücke.

Möchtest du tiefer in die Materie einsteigen?

- Clojure Regex-Dokumentation: https://clojuredocs.org/clojure.core/re-pattern
- RegExr: Eine interaktive Plattform zum Testen von regulären Ausdrücken: https://regexr.com/

Pro-Tipp: `re-seq` statt `find`

Wenn du mehrere Übereinstimmungen im selben Text finden möchtest, nutze die Funktion `re-seq` statt `find`. Dies liefert dir eine Sequenz aller gefundenen Übereinstimmungen statt einer einzigen.

Damit hast du alles, was du über reguläre Ausdrücke in Clojure wissen musst! Viel Spaß beim Manipulieren von Texten 🙂