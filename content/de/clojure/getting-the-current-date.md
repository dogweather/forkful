---
title:                "Das aktuelle Datum erhalten"
html_title:           "Clojure: Das aktuelle Datum erhalten"
simple_title:         "Das aktuelle Datum erhalten"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was ist es & Warum?
Die aktuellen Datum und Uhrzeit zu erhalten, ist ein häufiges Bedürfnis für viele Programmierer. Es ermöglicht die Verwendung von aktuellen Zeitangaben in Anwendungen und kann für die Datensynchronisation und -archivierung nützlich sein.

## Wie geht das?
Um das aktuelle Datum in Clojure zu erhalten, gibt es zwei Möglichkeiten:

1. Verwenden Sie die Funktion ```(java.util.Date.)```, um ein Java-Date-Objekt zu erstellen und dann die Funktion ```(.toString)``` anzuwenden, um das Datum als String auszugeben.

    ```Clojure
    (def my-date (java.util.Date.)) ; aktuelles Datum erstellen
    (.toString my-date) ; das Datum als String ausgeben
    ```
2. Verwenden Sie die Funktion ```(java.time.LocalDateTime/now)```, um ein Java-LocalDateTime-Objekt zu erstellen und dann die Funktion ```(.toString)``` anzuwenden, um das Datum als String auszugeben. Diese Methode ist mit Java 8 und höher kompatibel.

    ```Clojure
    (def my-date (java.time.LocalDateTime/now)) ; aktuelles Datum erstellen
    (.toSting my-date) ; das Datum als String ausgeben
    ```

Die Ausgabe wird in beiden Fällen folgendermaßen aussehen: ```Wed May 19 17:23:59 IST 2021```

## Tiefgehende Informationen
Die Verwendung der Funktion ```(java.util.Date.)``` ist seit Java 1.0 verfügbar und unterstützt nur Datumsangaben bis zum Jahr 2037. Aus diesem Grund empfehlen wir die Verwendung von ```(java.time.LocalDateTime/now)```, da diese Funktion auch mit höheren Jahren kompatibel ist.

Andere Möglichkeiten, das aktuelle Datum in Clojure zu erhalten, sind die Verwendung von Bibliotheken wie clj-time oder java-time, die zusätzliche Funktionen und Unterstützung für verschiedene Zeitzonen bieten.

## Siehe auch
- [clojuredocs.org/java.util.Date](https://clojuredocs.org/java.util.Date)
- [clojuredocs.org/java.time.LocalDateTime](https://clojuredocs.org/java.time.LocalDateTime)
- [github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
- [github.com/dmiller/clojure.java-time](https://github.com/dmiller/clojure.java-time)