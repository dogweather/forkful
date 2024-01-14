---
title:    "Clojure: Vergleich von zwei Daten"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Warum
Das Vergleichen von zwei Daten ist eine wichtige Fähigkeit beim Programmieren, um beispielsweise Zeitdifferenzen zu berechnen oder bestimmte Aktionen basierend auf dem Datum auszuführen.

## Wie geht das?
Um zwei Daten in Clojure zu vergleichen, kann die Funktion `compare` verwendet werden. Diese nimmt zwei Argumente entgegen und gibt einen Wert zurück, der angibt, ob das erste Datum kleiner, größer oder gleich dem zweiten Datum ist. Hier sind einige Beispiele:

```Clojure
;; Vergleich von Datumsobjekten
(compare (java.util.Date. 2020 3 15) (java.util.Date. 2020 3 10))
;; Output: 1 (das erste Datum ist größer)
(compare (java.util.Date. 2020 3 10) (java.util.Date. 2020 3 15))
;; Output: -1 (das erste Datum ist kleiner)
(compare (java.util.Date. 2020 3 15) (java.util.Date. 2020 3 15))
;; Output: 0 (die Daten sind gleich)

;; Vergleich von Strings mit Datumsformat 'yyyy-MM-dd'
(compare "2020-03-15" "2020-03-10")
;; Output: 1 (das erste Datum ist größer)
(compare "2020-03-10" "2020-03-15")
;; Output: -1 (das erste Datum ist kleiner)
(compare "2020-03-15" "2020-03-15")
;; Output: 0 (die Daten sind gleich)
```

Es ist wichtig zu beachten, dass die Funktion `compare` nur für Objekte verwendet werden kann, die das Protokoll `Comparable` implementieren, wie z.B. `java.util.Date` oder `java.time.LocalDate`.

## Tiefere Einblicke
Die Funktion `compare` nutzt intern die Funktionen `compare-numbers` oder `compare-strings`, abhängig vom Datentyp der übergebenen Argumente. Diese Funktionen vergleichen die einzelnen Komponenten eines Datums (Jahre, Monate, Tage) in aufsteigender Reihenfolge und geben den ersten Unterschied zurück. Falls alle Komponenten gleich sind, wird die Länge der Argumente verglichen. In der Clojure-Dokumentation können weitere Details zu diesen Funktionen gefunden werden.

## Siehe auch
- [Clojure-Dokumentation zu `compare`](https://clojuredocs.org/clojure.core/compare)
- [Clojurescript-Referenz zu Protokollen und Erweiterungen](https://clojurescript.org/reference/protocols#_comparable)
- [Clojure-Wiki zu `compare`](https://clojure.org/reference/datatypes#_comparable_data_types)