---
title:                "Das Parsen eines Datums aus einem String"
html_title:           "Clojure: Das Parsen eines Datums aus einem String"
simple_title:         "Das Parsen eines Datums aus einem String"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was ist das & Warum?
Das Parsen von Datum aus einem String ist der Prozess, bei dem ein Datumswert aus einem Text extrahiert wird. Programmierer tun dies, um mit Datumswerten in ihren Programmen zu arbeiten.

## Wie geht's?
```Clojure
; Ein Datum aus einem Text extrahieren
(-> "18. März 2021"           ; Der Text, aus dem das Datum extrahiert wird
    (parse-date "dd. MMM yyyy") ; Das Datum in gewünschtem Format angeben
    (.toInstant))              ; Das Datum als Instant Objekt ausgeben
; Ausgabe: #object[java.time.Instant 0x602b8967 "2021-03-18T00:00:00Z"]

; Ein Datum aus einem String parsen und als Date Objekt ausgeben
(ns my-date-parser.core
  (:import
   [java.time LocalDate]
   [java.time.format DateTimeFormatter]))
(def formatter (DateTimeFormatter/ofPattern "dd. MMM yyyy"))
(local-date "1. Januar 2021" formatter) ; Das Datum als LocalDate Objekt ausgeben
; Ausgabe: #object[java.time.LocalDate 0x72d8b880 "2021-01-01"]

;; Beachten Sie, dass die angegebene Formatierung dem Format des Datums im String entsprechen muss, sonst gibt es eine Exception.

## Tiefer in die Materie
Das Parsen von Datum aus einem String hat seinen Ursprung in der Notwendigkeit, Informationen in lesbarer Form darzustellen. Alternativen zum Parsen von Dateien aus Strings sind das manuelle Erstellen von Datumswerten oder die Verwendung von spezialisierten Bibliotheken.

Die Implementation von Datei-Parsing in Clojure nutzt die Java-Klasse "DateTimeFormatter", die verschiedene Methoden zum Parsen von Datumsangaben bereitstellt. Clojure bietet auch einige Wrapper-Funktionen, um den Prozess zu vereinfachen.

## Siehe auch
- [Java DateTimeFormatter Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Java LocalDate Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Clojure DateTimeUtils Bibliothek](https://github.com/clj-commons/clj-time)