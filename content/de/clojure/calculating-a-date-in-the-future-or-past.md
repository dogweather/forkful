---
title:                "Berechnung eines zukünftigen oder vergangenen Datums"
html_title:           "Clojure: Berechnung eines zukünftigen oder vergangenen Datums"
simple_title:         "Berechnung eines zukünftigen oder vergangenen Datums"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Berechnung von Datum in Clojure

## Was & Warum?
Berechnungen von Daten in der Zukunft oder Vergangenheit helfen dabei, Ereignisse zeitlich zu ordnen oder zu planen. Als Programmierer tun wir das, um Zeitabläufe zu koordinieren, Zeitraum-gesteuerte Analysen durchzuführen oder um Benachrichtigungen zu planen.

## Wie macht man das:
Mit der Clojure Bibliothek `clj-time`, lässt sich das ganz einfach realisieren. Im Folgenden finden Sie Codebeispiele:

```Clojure
(ns my-namespace
  (:require [clj-time.core :as t]
            [clj-time.format :as f]
            [clj-time.coerce :as c]))

(defn add-days [date-str days]
  (let [fmt (f/formatters :date-time-no-ms)
        date-time (c/from-string date-str)]
    (-> date-time
        (t/plus (t/days days))
        (f/unparse fmt))))

(println (add-days "2022-02-01T00:00:00Z" 7))
```
Die Ausgabe des Beispiels wäre: `2022-02-08T00:00:00Z`

## Deep Dive
Berechnungen für zukünftige oder vergangene Daten wurden lange vor Computern benötigt, alternativ wurden sie mit komplizierten Algorithmen durchgeführt. Mit der Erfindung von Computern und Programmiersprachen wie Clojure ist dies leichter geworden. Einige fassen diese Aufgabe mit Java's `LocalDate` oder `DateTime` an, andere verwenden `java.util.Date`, `java.util.Calendar` oder Jodatime. Die Bibliothek `clj-time` verwendet Jodatime und bietet eine funktionale und umgangssprachliche Syntax für die Arbeit mit Daten und Zeiten.

## Siehe auch
- Clojure [official documentation](https://clojure.org/api/api)
- [clj-time's Github page](https://github.com/clj-time/clj-time)
- [Java 8 Date & Time API tutorial](http://www.baeldung.com/java-8-date-time-intro)