---
date: 2024-01-20 17:36:13.647547-07:00
description: "How to: Clojure bietet `clj-time`, eine m\xE4chtige Bibliothek zur Datums-\
  \ und Zeitmanipulation. Hier ist ein einfaches Beispiel, wie man ein Datum in einen\u2026"
lastmod: '2024-03-13T22:44:53.431411-06:00'
model: gpt-4-1106-preview
summary: "Clojure bietet `clj-time`, eine m\xE4chtige Bibliothek zur Datums- und Zeitmanipulation."
title: Datum in einen String umwandeln
weight: 28
---

## How to:
Clojure bietet `clj-time`, eine mächtige Bibliothek zur Datums- und Zeitmanipulation. Hier ist ein einfaches Beispiel, wie man ein Datum in einen String konvertiert:

```Clojure
(require '[clj-time.format :as fmt])

;; Aktuelles Datum und Zeit
(def jetzt (t/now))

;; Formatierung definieren
(def formatter (fmt/formatter "dd.MM.yyyy HH:mm"))

;; Datum zu String konvertieren
(println (fmt/unparse formatter jetzt))
;; Beispielausgabe: "24.03.2023 15:45"
```

## Deep Dive
Früher nutzten Clojure-Entwickler Java-Interoperabilität, um mit `java.text.SimpleDateFormat` zu arbeiten. Jetzt gibt es `clj-time`, basierend auf Joda-Time, vor der Einführung von `java.time` in Java 8.

Alternative Ansätze umfassen die Nutzung von Java 8 `java.time` API direkt aus Clojure heraus oder das Einbinden anderer Bibliotheken wie `tick`. 

Die Implementierungsdetails betreffen Zeitzone (mit oder ohne), Lokalisierung und das Format. Die Verwendung der `java.util.Date` Instanz direkt wird weniger empfohlen, da `java.time` und `clj-time` reicher an Funktionen und weniger fehleranfällig sind.

## See Also
- Die `clj-time`-Dokumentation: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
- Offizielle Joda-Time Webseite: [https://www.joda.org/joda-time/](https://www.joda.org/joda-time/)
- `java.time` Dokumentation: [https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
