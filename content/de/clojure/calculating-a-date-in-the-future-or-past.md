---
title:                "Clojure: Berechnung eines Datums in der Vergangenheit oder Zukunft"
simple_title:         "Berechnung eines Datums in der Vergangenheit oder Zukunft"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von Daten in der Zukunft oder Vergangenheit ist eine nützliche Fähigkeit in der Programmierung, insbesondere in der Clojure-Sprache. Es kann bei der Erstellung von Kalender- oder Terminverwaltungsprogrammen oder bei der Berechnung von längeren Zeitspannen für statistische Analysen hilfreich sein.

## Wie

Um ein zukünftiges oder vergangenes Datum in Clojure zu berechnen, verwenden wir die "plus" Funktion aus dem "clojure.java.time" Paket. Wir geben das aktuelle Datum als Argument und verwenden dann die "plus" Funktion, um eine bestimmte Anzahl von Tagen, Monaten oder Jahren zu addieren oder subtrahieren.

```Clojure
(require '[clojure.java.time :as t])

;; Berechnung eines Datums in der Zukunft
(t/plus (t/local-date) {:days 5 :months 1 :years 2})
=> #object[java.time.LocalDate 0x2cf241f "04.09.2022"]

;; Berechnung eines Datums in der Vergangenheit
(t/plus (t/local-date) {:days -10 :months -2 :years -1})
=> #object[java.time.LocalDate 0x381d2f8f "17.02.2018"]
```

## Deep Dive

Die "plus" Funktion kann auch mit anderen Zeiteinheiten wie Stunden, Minuten oder Sekunden verwendet werden. Außerdem können wir auch spezifische Daten auswählen, z.B. das Datum in einer anderen Zeitzone berechnen.

```Clojure
;; Berechnung einer Uhrzeit in der Zukunft
(t/plus (t/local-time) {:hours 3 :minutes 40 :seconds 15})
=> #object[java.time.LocalTime 0x33a32e3e "12:33:26"]

;; Berechnung einer Uhrzeit in einer anderen Zeitzone
(t/plus (t/local-date-time (t/local-date)) {:minutes 30 :zone "Europe/Berlin"})
=> #object[java.time.LocalDateTime 0xc026dfd "30.07.2021T16:00:00"]
```

Es ist auch möglich, ein bestimmtes Datum oder eine bestimmte Uhrzeit direkt zu einem anderen Datum oder einer anderen Uhrzeit hinzuzufügen. Dazu verwenden wir die "plus-days", "plus-months", "plus-years", "plus-hours", "plus-minutes" und "plus-seconds" Funktionen.

```Clojure
(t/plus-days (t/local-date) 10)
=> #object[java.time.LocalDate 0x1572529e "08.08.2021"]

(t/plus-hours (t/local-time) 2)
=> #object[java.time.LocalTime 0xf57596d "19:02:13"]
```

## Siehe auch

- Offizielle Clojure-Dokumentation zu "clojure.java.time": https://clojure.github.io/java.time-api/

- Nützliche Bibliothek für Datum- und Uhrzeitberechnungen in Clojure: https://github.com/clj-time/clj-time