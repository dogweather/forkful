---
title:                "Clojure: Vergleich von zwei Daten"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Daten ist eine häufige Aufgabe in der Programmierung, besonders wenn es um die Verarbeitung von Datumsangaben geht. Es kann hilfreich sein, um zu überprüfen, ob ein bestimmtes Datum vor oder nach einem anderen Datum liegt oder um herauszufinden, wie viele Tage zwischen diesen beiden Daten liegen.

## Wie man zwei Daten vergleicht

In Clojure gibt es eine Vielzahl von Funktionen, die beim Vergleichen von Daten helfen können. Eine grundlegende Möglichkeit ist die Verwendung der Funktion `compare`, die zwei Daten miteinander vergleicht und ein Ergebnis zurückgibt, das angibt, ob das erste Datum vor, gleich oder nach dem zweiten Datum liegt.

```Clojure
(compare #inst "2021-01-01" #inst "2021-01-15")
;; Output: -1
```

In diesem Beispiel wird `-1` zurückgegeben, was bedeutet, dass das erste Datum vor dem zweiten liegt.

Eine weitere nützliche Funktion ist `days`, die die Anzahl der Tage zwischen zwei Daten zurückgibt.

```Clojure
(days #inst "2021-01-01" #inst "2021-01-15")
;; Output: 14
```

Dies ist besonders praktisch, wenn man wissen möchte, wie viele Tage zwischen zwei Ereignissen vergangen sind.

## Tiefere Einblicke

Das Vergleichen von Daten kann komplexer werden, wenn es um verschiedene Zeitzonen oder die Berücksichtigung von Uhrzeiten geht. Hier bieten sich Funktionen wie `local-date-time` oder `zoned-date-time` an, um sicherzustellen, dass die Vergleiche unter Berücksichtigung der gewünschten Zeitzone oder Uhrzeiten erfolgen.

Zusätzlich gibt es auch Funktionen wie `before?` und `after?`die eine boolesche Aussage zurückgeben, die angibt, ob das erste Datum vor oder nach dem zweiten Datum liegt.

```Clojure
(after? #inst "2021-01-01" #inst "2021-01-15")
;; Output: false
```

Es gibt auch die Möglichkeit, benutzerdefinierte Vergleichsfunktionen zu erstellen, die spezifischere Kriterien für den Vergleich von Daten festlegen.

## Siehe auch

- [Clojure-Dokumentation zu Datums- und Zeitfunktionen](https://clojure.org/reference/java_interop#date_and_time)
- [Zusätzliche Beispiele und Erläuterungen zum Vergleichen von Daten in Clojure](https://www.baeldung.com/java-compare-dates)
- [Weitere Informationen zur Arbeit mit Datumsangaben in Clojure](https://adityatannu.com/blog/clojure-dates/)