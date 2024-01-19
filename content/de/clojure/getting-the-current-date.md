---
title:                "Das aktuelle Datum abrufen"
html_title:           "Gleam: Das aktuelle Datum abrufen"
simple_title:         "Das aktuelle Datum abrufen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Das aktuelle Datum in Clojure: Wie und Warum

## Was & Warum?

Das Abrufen des aktuellen Datums ist einfach das Holen des momentanen Datums und der Uhrzeit, an dem/zu der dein Programm läuft. Programmierer tun dies oft, um Zeitstempel für Datenprotokollierung, allgemeine Verfolgung und Zeitgesteuerte Funktionen zu erzeugen.

## Wie:

Enter Clojure! Hier ist, wie man das aktuelle Datum abruft:

```clojure
(require '[clj-time.core :as t])

(defn get-current-date []
  (t/now))
```

Beispieloutput:

```clojure
(get-current-date)
=> #object[org.joda.time.DateTime 2022-01-20T16:55:36.144Z]
```
## Tief tauchen:

### Historischer Kontext
Die Verwendung des Joda-Time-Pakets für Datums- und Zeitoperationen war eine übliche Praxis in der Java-Welt, bevor Java 8 veröffentlicht wurde. Clojure, eine JVM-Sprache, folgte dem Trend und clj-time wurde erstellt.

### Alternativen
Alternativ könnten Sie auch die eingebaute Java-Funktionalität verwenden:

```clojure
(java.util.Date.)
```
Aber die clj-time-Bibliothek bietet mehr Funktionen und bessere Zeitzonentools.

### Implementierungsdetails
Im Hintergrund ruft `t/now` die Joda-Time-Methode `DateTime.now` auf, um den aktuellen Zeitstempel zu erzeugen.

## Siehe auch:
- Offizielle Dokumentation von Clojure: https://clojure.org/
- clj-time GitHub: https://github.com/clj-time/clj-time
- Joda-Time in Java: https://www.joda.org/joda-time/