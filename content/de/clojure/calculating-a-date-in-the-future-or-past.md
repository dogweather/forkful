---
title:    "Clojure: Berechnen eines Datums in der Zukunft oder Vergangenheit"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Warum

Das Berechnen von zukünftigen oder vergangenen Daten ist eine häufige Aufgabe beim Programmieren. Es kann hilfreich sein, um beispielsweise Geburtstage, Fälligkeitsdaten oder Ereignisse in einem Kalender zu verwalten.

## Wie man es macht

Die Clojure-Bibliothek `clj-time` bietet eine einfache Möglichkeit, zukünftige oder vergangene Daten zu berechnen. Zunächst muss die Bibliothek importiert werden:

```Clojure
(require '[clj-time.core :as t])
```

Um eine zukünftige oder vergangene Datumseinheit zu berechnen, muss `t/plus` und ein Zeitintervall wie `t/days`, `t/weeks`, `t/months` oder `t/years` zusammen mit einem Startdatum verwendet werden. Hier ist ein Beispiel, das das Datum 10 Tage in der Zukunft berechnet:

```Clojure
(t/plus (t/today) 10 t/days)
```

Das `t/today` wird durch das heutige Datum ersetzt und die Ausgabe ist ein Date-Objekt 10 Tage in der Zukunft. Ebenso kann das Datum in der Vergangenheit berechnet werden, indem das Zeitintervall negativ ist. Zum Beispiel berechnet das folgende Beispiel das Datum 2 Monate in der Vergangenheit:

```Clojure
(t/plus (t/today) -2 t/months)
```

## Tiefer Einblick

Mit der `clj-time`-Bibliothek können auch komplexere Berechnungen durchgeführt werden. Zum Beispiel kann `t/plus` auch ein Datum mit einem `DateTimeField`-Objekt verwenden, um spezifische Teile des Datums zu manipulieren. Hier ist ein Beispiel, das das Datum auf den nächsten Montag ab dem heutigen Datum setzt:

```Clojure
(t/plus (t/today) 1 (t/next-monday (t/show (t/today) :day-of-week)))
```

Dies geschieht, indem der `next-monday`-Funktion mit dem heutigen Datum und dem `show`-Funktion mit dem `:day-of-week`-Argument aufgerufen wird, um den Wochentag als Zahl zu erhalten.

Es gibt auch andere nützliche Funktionen in der `clj-time`-Bibliothek, wie `t/compare` zum Vergleichen von Daten und `t/seconds-between` zum Berechnen der Anzahl der Sekunden zwischen zwei Daten.

## Siehe auch

- [Clojure-Docs: clj-time](https://clojuredocs.org/clojure.java-time)
- [Clojure-Wiki: clj-time](https://github.com/clj-time/clj-time/wiki) 
- [Clojure-Backend: Zeitrechnung in Clojure Tutorial](https://clojurebackend.com/articles/zeitrechnung-in-clojure)