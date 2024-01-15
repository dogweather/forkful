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

# Warum

Viele Anwendungen erfordern die Berechnung von zukünftigen oder vergangenen Daten. Mit Clojure können Sie dies einfach und präzise tun. In diesem Artikel erfahren Sie, wie Sie mithilfe von Clojure schnell und effizient ein Datum in der Zukunft oder Vergangenheit berechnen können.

## Wie man ein Datum in der Zukunft oder Vergangenheit berechnet

Die Schlüsselkomponente, die in der Berechnung von zukünftigen oder vergangenen Daten verwendet wird, ist die Clojure-Funktion `java.util.Calendar`. Diese Funktion ermöglicht es Ihnen, ein Datum zu managen und verschiedene Berechnungen durchzuführen.

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, müssen wir zuerst eine Instanz von `java.util.Calendar` erstellen:

```Clojure
(def cal (java.util.Calendar/getInstance))
```

Als nächstes können wir mithilfe von `set` die gewünschten Felder des Kalenders ändern, wie zum Beispiel das Jahr, den Monat oder den Tag. Zum Beispiel, um ein Datum 10 Jahre in der Zukunft zu berechnen, können wir Folgendes tun:

```Clojure
(.set cal java.util.Calendar/YEAR (+ (.get cal java.util.Calendar/YEAR) 10))
```

Dies würde das aktuelle Jahr um 10 erhöhen. Wir können auch andere Felder wie `MONTH` oder `DAY_OF_MONTH` ändern, indem wir ihre entsprechenden Werte mit `+` oder `-` verändern.

Zuletzt müssen wir die neue Instanz von `java.util.Calendar` in ein Datum umwandeln, das von anderen Funktionen in Clojure verwendet werden kann. Dies kann mit der Funktion `time` durchgeführt werden:

```Clojure
(time (.getTime cal))
```

Dies gibt uns ein Datum in der Form eines `java.util.Date`-Objekts zurück.

## Tiefer Einblick

Mithilfe der `java.util.Calendar`-Funktion in Clojure können wir nicht nur zukünftige oder vergangene Daten berechnen, sondern auch mehrere andere Operationen durchführen, wie zum Beispiel das Vergleichen von Daten, das Hinzufügen von Zeitintervallen oder das Extrahieren von Informationen aus einem Datum.

Die vollständige Dokumentation zu `java.util.Calendar` kann unter folgendem Link gefunden werden: https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html

# Siehe auch

- https://clojuredocs.org/clojure.core/time
- https://dzone.com/articles/manipulating-dates-and-intervals-in-clojure
- https://www.brainonfire.net/files/2013-07-15/clojure-dates.pdf