---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:47.293630-07:00
description: "Wie geht das: Da Clojure eine JVM-Sprache ist, erm\xF6glicht sie Ihnen,\
  \ direkte Java's Datums- und Zeitbibliotheken zu verwenden. Beginnen wir mit der\u2026"
lastmod: '2024-03-13T22:44:53.429156-06:00'
model: gpt-4-0125-preview
summary: "Da Clojure eine JVM-Sprache ist, erm\xF6glicht sie Ihnen, direkte Java's\
  \ Datums- und Zeitbibliotheken zu verwenden."
title: Einen Datum aus einem String analysieren
weight: 30
---

## Wie geht das:
Da Clojure eine JVM-Sprache ist, ermöglicht sie Ihnen, direkte Java's Datums- und Zeitbibliotheken zu verwenden. Beginnen wir mit der eingebauten Java-Interoperation und erkunden dann, wie man eine beliebte Drittanbieter-Bibliothek, clj-time, für ideomatischere Clojure-Lösungen nutzen kann.

### Verwendung von Java Interop
Clojure kann direkt Java's `java.time.LocalDate` nutzen, um Daten aus Strings zu parsen:
```clojure
(require '[clojure.java.io :as io])

; Ein Datum mittels Java Interop parsen
(let [date-str "2023-04-01"
      date (java.time.LocalDate/parse date-str)]
  (println date))
; Ausgabe: 2023-04-01
```

### Verwendung von clj-time
Eine idiomatischere Clojure-Bibliothek zum Umgang mit Daten und Zeiten ist `clj-time`. Sie basiert auf Joda-Time, einer umfassenden Bibliothek für Datums- und Zeitoperationen. Zuerst müssen Sie `clj-time` zu den Abhängigkeiten Ihres Projekts hinzufügen. Hier sehen Sie, wie Sie einen Datumsstring mit `clj-time` parsen:

```clojure
; Stellen Sie sicher, dass Sie [clj-time "0.15.2"] in Ihrem project.clj unter :dependencies hinzufügen

(require '[clj-time.format :as fmt]
         '[clj-time.core :as time])

; Definieren Sie einen Formatierer
(let [formatter (fmt/formatter "yyyy-MM-dd")
      date-str "2023-04-01"
      parsed-date (fmt/parse formatter date-str)]
  (println parsed-date))
; Ausgabe: #object[org.joda.time.DateTime 0x76eccb5d "2023-04-01T00:00:00.000Z"]
```

Diese Beispiele demonstrieren das grundlegende Parsen von Daten. Beide Methoden sind nützlich, aber `clj-time` kann mit zusätzlichen Funktionen für komplexe Anforderungen einen mehr auf Clojure zentrierten Ansatz bieten.
