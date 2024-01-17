---
title:                "Berechnung eines Datums in der Zukunft oder Vergangenheit"
html_title:           "Clojure: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Berechnung eines Datums in der Vergangenheit oder Zukunft bezieht sich auf die Bestimmung eines Datums, das eine bestimmte Anzahl von Tagen, Monaten oder Jahren vor oder nach einem angegebenen Datum liegt. Programmierer nutzen dies häufig, um beispielsweise Fristen oder Ereignisse zu berechnen.

## So geht's:
Das Berechnen eines Datums in der Zukunft oder Vergangenheit ist in Clojure einfach dank der Funktion `clj-time`, die Teil der Bibliothek `clj-time-lite` ist. Hier ist ein Beispiel, um das Datum von heute plus 30 Tagen zu berechnen:
```Clojure
(require '[clj-time.core :as time])
(time/plus (time/today) 30 :days)
```
Das Ausgabeformat ist ein `DateTime` Objekt, das durch das `clj-time` Modul bereitgestellt wird.

## Tiefer Einblick:
Das Berechnen von Datumsangaben ist ein wichtiger Bestandteil in der Programmierung, insbesondere im Zusammenhang mit Datum und Zeit. Es gibt auch andere Möglichkeiten, um das Datum in der Zukunft oder Vergangenheit zu berechnen, wie z.B. die Verwendung von Java's `java.util.Calendar` Klasse. Jedoch ist die Verwendung von `clj-time` in Clojure eine effizientere und einfachere Methode. Die `clj-time-lite` Bibliothek bietet auch weitere nützliche Funktionen und Arten, um mit Datum und Zeit umzugehen.

## Siehe auch:
- [Dokumentation für `clj-time`](https://github.com/clj-time/clj-time)
- [Offizielle Clojure Webseite](https://clojure.org/)
- [Andere nützliche Clojure Bibliotheken](https://github.com/razum2um/awesome-clojure)