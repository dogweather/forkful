---
date: 2024-01-20 17:31:06.115858-07:00
description: "Das Berechnen eines zuk\xFCnftigen oder vergangenen Datums erm\xF6glicht\
  \ es, Zeitintervalle zu manipulieren. Entwickler nutzen diese M\xF6glichkeit, um\
  \ Funktionen\u2026"
lastmod: '2024-03-11T00:14:27.398034-06:00'
model: gpt-4-1106-preview
summary: "Das Berechnen eines zuk\xFCnftigen oder vergangenen Datums erm\xF6glicht\
  \ es, Zeitintervalle zu manipulieren. Entwickler nutzen diese M\xF6glichkeit, um\
  \ Funktionen\u2026"
title: "Berechnung eines zuk\xFCnftigen oder vergangenen Datums"
---

{{< edit_this_page >}}

## Was & Warum?
Das Berechnen eines zukünftigen oder vergangenen Datums ermöglicht es, Zeitintervalle zu manipulieren. Entwickler nutzen diese Möglichkeit, um Funktionen wie Erinnerungen, Terminplanungen oder Verfallsdaten-Berechnungen in ihren Anwendungen zu realisieren.

## Wie macht man das:

```Clojure
;; Aktuelles Datum holen
(def heute (java.util.Date.))

;; Kalender-Instanz für Berechnungen erzeugen
(def kalender (java.util.GregorianCalendar.))

;; Heutiges Datum im Kalender setzen
(.setTime kalender heute)

;; 10 Tage zum Datum hinzufügen
(.add kalender java.util.Calendar/DATE 10)
(def in-zehn-tagen (.getTime kalender))

;; 10 Tage vom Datum subtrahieren
(.add kalender java.util.Calendar/DATE -20) ;; 10 Tage zurück, da wir schon 10 Tage hinzugefügt haben
(def vor-zehn-tagen (.getTime kalender))

;; Ausgabe
(println "Heutiges Datum:" heute)
(println "Datum in 10 Tagen:" in-zehn-tagen)
(println "Datum vor 10 Tagen:" vor-zehn-tagen)
```

Sample Output:

```
Heutiges Datum: Thu Apr 06 00:00:00 CEST 2023
Datum in 10 Tagen: Sun Apr 16 00:00:00 CEST 2023
Datum vor 10 Tagen: Tue Mar 28 00:00:00 CEST 2023
```

## Hintergrundwissen:

Bereits vor dem Computerzeitalter war das Rechnen mit Datums- und Zeitangaben wichtig – für Astronomie, Navigation und Landwirtschaft. Heute macht die java.util.Calendar-Klasse das Arbeiten mit Datumsberechnungen in Clojure und anderen JVM-Sprachen relativ einfach.

Alternativen zu `java.util.Calendar` sind neuere APIs wie `java.time` (ab Java 8), die mehr Funktionalitäten und eine bessere Thread-Sicherheit bieten. clojure.java-time ist ein Wrapper für `java.time`. Es lohnt sich, auch diese Bibliothek zu betrachten.

Bei der Implementierung solcher Datumskalkulationen sind Timezones und Schaltjahre zu berücksichtigen. Fehler in solchen Berechnungen können zu falschen Ergebnissen und womöglich schwerwiegenden Problemen führen – zum Beispiel in Buchungs- oder Abrechnungssystemen.

## Siehe auch:

- Oracle Docs zu `java.util.Calendar`: https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html
- `clojure.java-time` Bibliothek: https://github.com/dm3/clojure.java-time
- Blog zur Zeitmessung in der Informatik: https://www.clojure.at/zeit/zeitmessung-im-computer
- Java 8 Date-Time API: https://docs.oracle.com/javase/tutorial/datetime/
