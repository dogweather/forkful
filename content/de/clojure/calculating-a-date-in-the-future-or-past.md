---
title:                "Clojure: Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von Datum in der Vergangenheit oder Zukunft kann in vielen Fällen sehr nützlich sein, z.B. um den Liefertermin für ein Paket zu bestimmen oder um eine Erinnerung für einen Termin in Zukunft zu setzen. Um diese Aufgabe mit Leichtigkeit zu bewältigen, ist es hilfreich, ein klares Verständnis der Programmiersprache Clojure zu haben.

## Wie man es macht

Um ein Datum in der Zukunft zu berechnen, können wir die "plus" Funktion verwenden. Angenommen, wir möchten 30 Tage zum aktuellen Datum hinzufügen, können wir dies wie folgt tun:

```Clojure
(plus (local-date) (days 30))
```

Dies gibt uns das Datum 30 Tage nach dem heutigen Datum zurück. Wir können auch negative Werte verwenden, um ein Datum in der Vergangenheit zu berechnen. Zum Beispiel, um das Datum vor 2 Monaten zu erhalten, können wir Folgendes tun:

```Clojure
(plus (local-date) (months -2))
```

Dies gibt uns das Datum vor 2 Monaten zurück.

## Tiefer tauchen

Clojure hat auch integrierte Funktionen, um bestimmte Tage in einer Woche oder Monat zu berechnen. Zum Beispiel die Funktion "day-of-week" gibt uns einen numerischen Wert für den Wochentag eines bestimmten Datums zurück, wobei 1 für Sonntag und 7 für Samstag steht. Die Funktion "month-of-year" gibt uns den numerischen Wert für den Monat eines Datums zurück, wobei 1 für Januar und 12 für Dezember steht.

```Clojure
(day-of-week (local-date)) ;; Gibt uns den numerischen Wert für den heutigen Wochentag zurück
(month-of-year (local-date)) ;; Gibt uns den numerischen Wert für den aktuellen Monat zurück
```

Mit diesen Funktionen können wir noch präziser ein Datum in der Vergangenheit oder Zukunft berechnen. Zum Beispiel, um den letzten Montag des nächsten Monats zu erhalten, können wir dies tun:

```Clojure
(plus (last (monday-of-month (local-date) (plus (local-date) (months 1)))))
```

Dieses Beispiel mag komplex erscheinen, aber es zeigt, dass die Kombination verschiedener Funktionen einen genaueren Ergebnis liefern kann.

## Siehe auch

- [Clojure-Dokumentation zu Datumsfunktionen](https://clojure.github.io/clojure/java-time/index.html)
- [Beispiele für Datumskalkulationen mit Clojure](https://www.baeldung.com/java-8-date-time-intro)