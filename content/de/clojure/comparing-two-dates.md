---
title:                "Vergleich von zwei Daten"
html_title:           "Clojure: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

Was & Warum?

Beim Vergleichen von zwei Daten in der Programmierung geht es darum, herauszufinden, welches Datum früher oder später ist. Dies ist besonders nützlich, wenn man beispielsweise verschiedene Ereignisse in einer bestimmten Reihenfolge sortieren möchte. Programmierer nutzen dieses Konzept häufig, um die Logik ihres Codes zu verbessern.

Wie?

Um zwei Daten in Clojure zu vergleichen, können wir die Funktion `compare` verwenden. Diese Funktion gibt entweder -1, 0 oder 1 zurück, abhängig davon, ob das erste Datum früher, gleich oder später als das zweite Datum ist.

```Clojure
;; Beispiel:
(compare (java.util.Date. 2020 1 1) (java.util.Date. 2020 1 2))

;; Output:
-1
```

Wir können auch die Funktionen `before?` und `after?` verwenden, um zu überprüfen, ob ein Datum vor oder nach einem anderen Datum liegt. Diese Funktionen geben einen booleschen Wert zurück.

```Clojure
;; Beispiel:
(before? (java.util.Date. 2020 1 1) (java.util.Date. 2020 1 2))

;; Output:
true
```

Tiefer Einblick

Das Konzept des Vergleichens von Daten ist nicht neu und wird in vielen Programmiersprachen verwendet. Es ist jedoch wichtig zu beachten, dass es bei verschiedenen Datentypen, wie zum Beispiel Strings oder Integers, unterschiedliche Implementierungen geben kann. Daher ist es immer wichtig, die Dokumentation der verwendeten Funktionen zu lesen.

Das Vergleichen von Daten kann auch durch andere Methoden wie dem Überladen von Vergleichsoperatoren oder der Verwendung von Bibliotheken wie Joda Time erreicht werden. Diese Alternativen können in bestimmten Fällen effektiver sein.

Siehe auch

- Offizielle Dokumentation für Clojure's `compare` Funktion: https://clojuredocs.org/clojure.core/compare
- Offizielle Dokumentation für Clojure's `before?` Funktion: https://clojuredocs.org/clojure.core/before_q
- Offizielle Dokumentation für Clojure's `after?` Funktion: https://clojuredocs.org/clojure.core/after_q
- Joda Time Bibliothek: https://www.joda.org/joda-time/