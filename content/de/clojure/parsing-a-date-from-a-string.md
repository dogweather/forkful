---
title:                "Einen Datum aus einem String parsen"
html_title:           "Elixir: Einen Datum aus einem String parsen"
simple_title:         "Einen Datum aus einem String parsen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Clojure-Datumsanalyse: Wie man ein Datum aus einem String in Clojure liest

## Was & Warum
Das Parsen eines Datums aus einem String ist der Prozess, einen String zu analysieren und in ein Datumsobjekt umzuwandeln. Dies ermöglicht es Programmierern, Datumsdaten leicht zu manipulieren und zu vergleichen.

## So geht's
Wir verwenden die Java-Interoperabilität von Clojure und die Java-`SimpleDateFormat`-Klasse, um dieses Problem zu lösen.

```Clojure
(import 'java.text.SimpleDateFormat)

(defn parse-date [s]
  (let [df (SimpleDateFormat. "dd-MM-yyyy")]
    (.parse df s)))

(println (parse-date "20-12-2020"))
```
Wenn dieser obige Code ausgeführt wird, wird er `Sun Dec 20 00:00:00 CET 2020` ausgeben.

## Genauer betrachtet
Historisch sehen wir, dass die Datumsformatierung und das Parsen in vielen Programmiersprachen ein gebräuchliches Problem darstellen. Alternativ zu unserem Ansatz könnten wir auch auf Clojure-Bibliotheken wie `clj-time` oder `java.time.*` zurückgreifen, die ab Java 8 verfügbar sind.

Bitte beachten Sie, dass beim Parsen eines Datums Zeitzonen berücksichtigt werden müssen. `SimpleDateFormat` analysiert das Datum standardmäßig in der lokalen Zeitzone, es sei denn, Sie stellen eine andere ein.

## Siehe auch
Um Ihre Kenntnisse zu erweitern, sollten Sie diese Links durchschauen:

- Clojure Java-Interop (Offizielle Dokumentation) : https://clojure.org/reference/java_interop
- SimpleDateFormat Dokumentation: https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html
- Clj-Time, eine Clojure Bibliothek für Datum und Zeit : https://github.com/clj-time/clj-time
- Java Time Dokumentation : https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html

Dies ist eine einfache, aber verbreitete Aufgabe in vielen Anwendungen. Hoffentlich gibt Ihnen dieser kleine Leitfaden ein besseres Verständnis, wie Sie dies in Clojure erreichen können.