---
title:    "Clojure: Zwei Daten vergleichen"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Warum

Eine häufige Aufgabe beim Programmieren ist es, zwei Daten miteinander zu vergleichen. Dies kann nützlich sein, um zum Beispiel zu überprüfen, ob ein Benutzer seit seinem letzten Besuch auf deiner Webseite irgendwelche Änderungen vorgenommen hat. In Clojure gibt es verschiedene Möglichkeiten, um dies zu tun, und in diesem Artikel werden wir uns genauer damit beschäftigen.

# Wie geht das?

Es gibt zwei grundlegende Wege, um zwei Daten in Clojure zu vergleichen: mit `=` und `compare`.

```Clojure
(= "Hallo" "Hallo") ; => true
(= 4 4) ; => true
(= 5 3) ; => false
(= "Clojure" "Java") ; => false
```

Der Operator `=` vergleicht die Werte der beiden Argumente und gibt `true` zurück, wenn sie gleich sind, und `false` wenn nicht. Beachte, dass `=` nicht zwischen verschiedenen Datentypen unterscheidet, so dass sowohl 4 als auch "4" als gleich betrachtet werden.

```Clojure
(compare 5 6) ; => -1
(compare "Katze" "Hund") ; => -1
(compare "Fisch" "Fisch") ; => 0
(compare 10 5) ; => 1
(compare 4 4) ; => 0
```

Die `compare` Funktion vergleicht die Argumente in einer bestimmten Ordnung und gibt eine negative Zahl zurück, wenn das erste Argument kleiner ist, eine positive Zahl, wenn es größer ist und 0, wenn sie gleich sind. Diese Funktion ist nützlich, wenn du die relative Reihenfolge von Daten bestimmen möchtest.

# Tiefergehende Informationen

Clojure hat auch die `time` Bibliothek, die verschiedene hilfreiche Funktionen für das Arbeiten mit Datum und Uhrzeit bietet. Eine davon ist `before?`, die prüft, ob ein Datum vor einem anderen liegt.

```Clojure
(require '[clojure.java-time :as t])

(t/before? (t/local-date "2020-01-01") (t/local-date "2020-05-01")) ; => true
(t/before? (t/local-date "2020-05-01") (t/local-date "2020-01-01")) ; => false
```

Du kannst auch die `between` Funktion verwenden, um herauszufinden, wie viele Tage, Monate oder Jahre zwischen zwei Daten liegen.

```Clojure
(t/between (t/local-date "2019-01-01") (t/local-date "2020-01-01") :days) ; => 365
(t/between (t/local-date "2019-01-01") (t/local-date "2020-01-01") :months) ; => 12
(t/between (t/local-date "2019-01-01") (t/local-date "2020-01-01") :years) ; => 1
```

# Siehe auch

- [Clojure Java Time Library Documentation](https://cljdoc.org/d/java-time/java-time/0.3.2/doc/readme)
- [Offizielle Clojure Dokumentation](https://clojure.org/documentation)
- [Learn X in Y minutes - Clojure](https://learnxinyminutes.com/docs/clojure/)