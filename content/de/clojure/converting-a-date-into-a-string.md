---
title:                "Clojure: Ein Datum in einen String umwandeln"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von Daten in Zeichenketten ist eine häufige Aufgabe beim Programmieren, die besonders in der Webentwicklung relevant ist. Es kann hilfreich sein, ein Datum in einer bestimmten Formatierung auszugeben, um es beispielsweise in einer Benutzeroberfläche darzustellen oder in einer Datenbank zu speichern. In diesem Artikel werden wir uns ansehen, wie man mithilfe von Clojure ein Datum in eine Zeichenkette umwandeln kann.

## Wie geht das?

Um ein Datum in eine Zeichenkette zu konvertieren, benötigen wir zunächst das "clj-time" Paket von Clojure. Wir importieren es einfach in unser Programm und können dann die Funktion "format" verwenden, um eine beliebige Datumsangabe in ein bestimmtes Format umzuwandeln.

```Clojure
(require '[clj-time.core :as time])

;; Konvertierung von einem Datum in das Format TT.MM.JJJJ
(time/format (time/date-time 2020 11 15) "dd.MM.yyyy")
;; Output: "15.11.2020"

;; Konvertierung von einem Datum und Uhrzeit in das Format TT.MM.JJJJ, hh:mm
(time/format (time/date-time 2020 11 15 13 30) "dd.MM.yyyy, HH:mm")
;; Output: "15.11.2020, 13:30"
```

Hier sehen wir, dass wir durch die Verwendung von "dd" für den Tag, "MM" für den Monat und "yyyy" für das Jahr jedes Datum in das gewünschte Format bringen können. Ebenso können wir mit "HH" und "mm" für Stunden und Minuten das Datum und die Uhrzeit kombinieren.

## Tiefere Einblicke

Es gibt viele verschiedene Optionen, die wir in unserem Format-String verwenden können, um ein Datum in eine Zeichenkette zu konvertieren. Zum Beispiel können wir auch Wochentage, Zeitzonen und sogar Schaltjahre berücksichtigen. Hier sind einige häufig verwendete Optionen:

- "EEEE": Gibt den Namen des Wochentags aus, zum Beispiel "Sonntag"
- "zz": Gibt die Zeitzone aus, zum Beispiel "+0100" für Mitteleuropäische Zeit
- "LLLL": Gibt den vollen Namen des Monats aus, zum Beispiel "November"
- "YYYY": Gibt das Jahr mit vier Ziffern aus, zum Beispiel "2020"
- "yy": Gibt das Jahr mit zwei Ziffern aus, zum Beispiel "20"
- "MM": Gibt den Monat mit zwei Ziffern aus, zum Beispiel "11"

Es ist immer eine gute Idee, die offizielle Dokumentation von clj-time (https://github.com/clj-time/clj-time) zu Rate zu ziehen, um alle verfügbaren Optionen und ihre Bedeutung zu finden.

## Siehe auch

- https://github.com/clj-time/clj-time - Das offizielle Paket für die Arbeit mit Datum und Uhrzeit in Clojure.
- https://clojuredocs.org/clj-time.core/format - Eine ausführliche Dokumentation zur "format" Funktion von clj-time.
- https://daily-javascript.com/articles/converting-dates-in-clojure/ - Ein Artikel zum Konvertieren von Daten in Zeichenketten in Clojure, der zusätzlich einige hilfreiche Tipps gibt.