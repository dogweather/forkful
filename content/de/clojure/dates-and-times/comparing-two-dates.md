---
date: 2024-01-20 17:32:28.099360-07:00
description: 'So geht''s: .'
lastmod: '2024-03-13T22:44:53.432324-06:00'
model: gpt-4-1106-preview
summary: .
title: Vergleich von zwei Daten
weight: 27
---

## So geht's:
```Clojure
;; Importiere das java.util.Date
(import 'java.util.Date)

;; Erstelle zwei Daten
(def date1 (Date.))
(Thread/sleep 1000) ;; Warte 1 Sekunde
(def date2 (Date.))

;; Vergleiche die Daten
(println (.before date1 date2)) ;; date1 ist vor date2 -> true
(println (.after date1 date2))  ;; date1 ist nach date2 -> false
(println (.equals date1 date2)) ;; date1 ist gleich date2 -> false

;; Nutze clj-time für mehr Funktionalität
(require '[clj-time.core :as t])
(require '[clj-time.coerce :as c])

;; Erstelle zwei Joda-Time-Daten
(def joda-date1 (c/to-date-time date1))
(def joda-date2 (c/to-date-time date2))

;; Vergleiche die Joda-Time-Daten
(println (.isBefore joda-date1 joda-date2)) ;; true
(println (.isAfter joda-date1 joda-date2))  ;; false
(println (.isEqual joda-date1 joda-date2))  ;; false
```
Sample Output:
```
true
false
false
true
false
false
```

## Deep Dive:
Historisch gesehen verwendeten Clojure-Entwickler oft die Java-Datums-API direkt, da Clojure auf der JVM läuft. Alternativ gibt es die `clj-time` Bibliothek, die auf der Joda-Time Bibliothek basiert und eine reichhaltigere API bietet. Bei der Implementierung ist zu beachten, dass Vergleiche auf Millisekunden-Genauigkeit beruhen und Zeitzone sowie Sommer-/Winterzeit Einfluss auf das Ergebnis haben können.

## Siehe auch:
- [`clj-time` GitHub-Seite](https://github.com/clj-time/clj-time)
- [Java Date API Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [ClojureDocs, eine Community-basierte Clojure-Dokumentation](https://clojuredocs.org/)
