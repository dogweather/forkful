---
title:    "Clojure: Unterstrings extrahieren"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Warum

Substrings zu extrahieren kann in vielen Situationen nützlich sein, besonders wenn man mit Texten arbeitet. Zum Beispiel könnte man bestimmte Wörter oder Buchstaben aus einem längeren Text herausfiltern oder Teilstrings vergleichen.

## Wie man es macht

Um Substrings in Clojure zu extrahieren, gibt es verschiedene Funktionen, die wir nutzen können. Eine davon ist `subs`, welche es uns erlaubt, einen Teil eines Strings anhand von Start- und Endpositionen zu extrahieren.

```Clojure
(def text "Hallo Welt!")

(subs text 0 5)
;; Output: Hallo
```

In diesem Beispiel nutzen wir die `subs` Funktion, um die ersten fünf Buchstaben des Strings auszugeben. Die erste Positionsangabe ist dabei inklusiv, während die zweite exklusiv ist.

Eine andere nützliche Funktion ist `substring`, welche es uns erlaubt, anhand von Indexpositionen zu extrahieren. Dies ist besonders hilfreich, wenn man die Länge des Strings nicht genau kennt.

```Clojure
(def text "Guten Abend!")

(substring text 6)
;; Output: Abend!
```

In diesem Fall geben wir einen Startindex an, ab dem der Substring extrahiert werden soll. Alle Zeichen ab diesem Index bis zum Ende werden dann ausgegeben.

## Tiefer Einblick

Beim Extrahieren von Substrings gibt es noch einige Details zu beachten. Zum Beispiel können wir auch negative Indexpositionen angeben, um von hinten zu zählen.

```Clojure
(def text "Mein Name ist Bob.")

(subs text -4)
;; Output: Bob.
```

In diesem Beispiel geben wir einen negativen Index an, was bedeutet, dass der vierte Buchstabe von hinten gezählt wird. Dadurch wird der letzte Teil des Strings extrahiert.

Außerdem können wir auch mit regulären Ausdrücken arbeiten, um bestimmte Muster in einem String zu finden und zu extrahieren.

```Clojure
(def text "Solar System: Mercury, Venus, Earth")

(re-seq #"[A-Z][a-z]+" text)
;; Output: ("Solar" "System" "Mercury" "Venus" "Earth")
```

In diesem Beispiel nutzen wir die `re-seq` Funktion, um alle Wörter im String zu extrahieren, die mit einem Großbuchstaben beginnen und dann einen oder mehrere Kleinbuchstaben enthalten.

## Siehe auch

- Offizielle Dokumentation zu `subs`: https://clojuredocs.org/clojure.core/subs
- Offizielle Dokumentation zu `substring`: https://clojuredocs.org/clojure.core/substring
- Offizielle Dokumentation zu regulären Ausdrücken: https://clojuredocs.org/clojure.core/re-pattern