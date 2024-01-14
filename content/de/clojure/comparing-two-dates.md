---
title:                "Clojure: Vergleich von zwei Datumsangaben"
simple_title:         "Vergleich von zwei Datumsangaben"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Warum

Das Vergleichen von zwei Daten ist ein wichtiger Aspekt in der Programmierung, da es uns ermöglicht zu überprüfen, ob ein bestimmtes Datum vor oder nach einem anderen Datum liegt. Dies kann hilfreich sein, um Daten zu sortieren oder um bestimmte Aktionen basierend auf dem Datum auszuführen.

## Wie geht man vor

Um zwei Daten in Clojure zu vergleichen, können wir die Funktion `before?` oder `after?` verwenden. Diese Funktionen haben jeweils zwei Argumente, das erste Datum und das zweite Datum, und geben entweder `true` oder `false` zurück, je nachdem ob das erste Datum vor oder nach dem zweiten Datum liegt.

```Clojure
(before? (java.time.LocalDate/now) (java.time.LocalDate/parse "2020-01-01"))

; => true

(after? (java.time.LocalDate/now) (java.time.LocalDate/parse "2020-01-01"))

; => false
```

Wir können auch die Funktion `compare` verwenden, die uns eine Zahl zurückgibt, die angibt, ob das erste Datum vor, gleich oder nach dem zweiten Datum liegt.

```Clojure
(compare (java.time.LocalDate/now) (java.time.LocalDate/parse "2020-01-01"))

; => 1

(compare (java.time.LocalDate/parse "2020-01-01") (java.time.LocalDate/parse "2020-01-01"))

; => 0

(compare (java.time.LocalDate/parse "2020-01-01") (java.time.LocalDate/now))

; => -1
```

## Tieferer Einblick

Beim Vergleichen von Daten in Clojure ist es wichtig zu beachten, dass die Funktionen `before?`, `after?` und `compare` nur mit Java-Datentypen funktionieren. Wenn wir beispielsweise zwei Clojure-Datentypen wie `LocalDate` vergleichen möchten, müssen wir sie zunächst in Java-Datentypen umwandeln.

```Clojure
(before? (clj-time.core/now) (clj-time.core/parse "2020-01-01"))

; => ClassCastException java.lang.Long cannot be cast to java.time.LocalDate

(before? (clj-time.coerce/to-java (clj-time.core/now)) (clj-time.coerce/to-java (clj-time.core/parse "2020-01-01")))

; => true
```

## Siehe auch

- [Clojure Dokumentation über Datum und Zeit](https://clojure.org/reference/date_and_time)
- [Java 8 Date and Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)