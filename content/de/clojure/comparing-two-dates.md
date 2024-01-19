---
title:                "Vergleich von zwei Daten"
html_title:           "C#: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Vergleich von zwei Daten in Clojure

## Was & Warum?

Beim Vergleich zweier Daten überprüft man, ob ein Datum vor oder nach dem anderen liegt, oder ob sie identisch sind. Das ist wichtig, um Ereignisse zeitlich einzuordnen und um Zeitabläufe zu managen.

## Wie?

In Clojure verwenden wir die eingebaute Funktion `compare` zum Vergleich von zwei Daten.

```clojure
(require '[clj-time.core :as t]   
         '[clj-time.coerce :as c])

(def date1 (c/to-date-time "2022-01-01T12:00:00Z"))
(def date2 (c/to-date-time "2022-02-01T12:00:00Z"))

(compare date1 date2) ; -1
```

In diesem Beispiel gibt die Funktion `compare` -1 zurück, weil das erste Datum vor dem zweiten liegt.

## Tiefere Informationen

Historisch gesehen gibt es in vielen Sprachen Funktionen zum Vergleich von Daten. Clojure verwendet jedoch eine einheitliche und einfache Methode.

Alternativen zum direkten Vergleich mit `compare` könnten Funktionen wie `before?` oder `after?` aus dem clj-time-Paket sein. Diese liefern boolesche Werte und können in einigen Fällen einfacher zu verwenden sein.

Die Implementierungsdetails des Datenvergleichs in Clojure sind ziemlich einfach, da sie die Java-Funktion `Date.compareTo` intern verwenden. Daten werden in Millisekunden seit dem 01.01.1970 (Unix-Zeit) angegeben.

## Siehe auch

Für mehr Beispiele und weitere Informationen, siehe:

- [Clojure Dokumentation](https://clojure.github.io/clojure/)
- [clj-time Readme auf Github](https://github.com/clj-time/clj-time)
- [Java Date.compareTo Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html#compareTo-java.util.Date-)