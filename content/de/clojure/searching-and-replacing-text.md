---
title:                "Suchen und Ersetzen von Text"
date:                  2024-01-20T17:57:23.485862-07:00
model:                 gpt-4-1106-preview
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Was & Warum?
Textsuche und -ersatz ist ein Vorgang, bei dem spezifische Textfragmente gefunden und durch andere ersetzt werden. Programmierer nutzen diese Funktion, um Daten zu aktualisieren, Fehler zu korrigieren oder Informationen zu reorganisieren.

## So geht's:
```Clojure
;; Text ersetzen mit `clojure.string/replace`
(require '[clojure.string :as str])

;; Einfaches Beispiel: Ersetzt "Welt" durch "Clojure-Welt"
(str/replace "Hallo Welt" "Welt" "Clojure-Welt")
;; => "Hallo Clojure-Welt"

;; Mit regulären Ausdrücken: Ersetzt alle Ziffern durch "#"
(str/replace "R2D2 und C3PO" #"\d" "#")
;; => "R#D# und C#PO"

;; Groß- und Kleinschreibung ignorieren:
(str/replace "Clojure ist toll" #"(?i)clojure" "LISP")
;; => "LISP ist toll"
```

## Deep Dive
Die Such- und Ersetzungsfunktion in Clojure basiert auf Java's `String` Klassenmethoden, erweitert um Clojure's Unveränderlichkeitsprinzipien. Historisch gesehen haben fast alle Programmiersprachen solche Funktionen, da Textverarbeitung zu den grundlegenden Aufgaben gehört. Alternativen in anderen Sprachen sind beispielsweise Perl's mächtige Textverarbeitungsmöglichkeiten oder Python's `re` Modul. In Clojure hebt `clojure.string/replace` sich durch seine Einfachheit und die nahtlose Integration von regulären Ausdrücken hervor.

Die Funktion `clojure.string/replace` kann zwei Arten von Argumenten für den Suchstring annehmen:
1. Ein einfacher String, der wörtlich genommen wird.
2. Ein regulärer Ausdruck, der eine vielseitige Suche ermöglicht, einschließlich pattern matching und optionen wie case insensitivity.

Die Unveränderlichkeit von Daten in Clojure bedeutet, dass das Original nicht verändert wird, sondern eine neue Zeichenkette zurückgegeben wird.

## See Also
- Die offizielle Clojure-Dokumentation zu `clojure.string`: https://clojure.github.io/clojure/clojure.string-api.html
- Ein Tutorial zur Clojure String-Manipulation: https://clojurebridge.github.io/community-docs/docs/clojure/string/
- Java Pattern Klasse (für reguläre Ausdrücke): https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html