---
title:                "Datum aus einem String parsen"
date:                  2024-01-20T15:35:34.925179-07:00
html_title:           "Arduino: Datum aus einem String parsen"
simple_title:         "Datum aus einem String parsen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Datumsparsing konvertiert Datum-Strings in ein standardisiertes Format oder ein Datum-Objekt, worauf wir leichter operieren können. Programmierer machen das, um Daten konsistent zu verarbeiten und zu vergleichen.

## Wie geht das?
In Clojure nutzen wir oft die Java-Interoperabilität, um Datumsstrings zu parsen. Hier ein einfacher Weg mit `java.time`:

```clojure
(require '[java-time :as jt])

(defn parse-date [date-string]
  (jt/local-date "yyyy-MM-dd" date-string))

;; Beispiel:
(parse-date "2023-03-15")
;; => #object[java.time.LocalDate 0x536bf4c2 "2023-03-15"]
```

Man kann auch clj-time oder java.text.SimpleDateFormat verwenden, je nach Bedarf.

## Tiefergehende Informationen
Das Parsen von Datumsangaben geht weit zurück und ist essenziell, da Menschen Daten weltweit in unterschiedlichsten Formaten teilen. Standardbibliotheken in vielen Sprachen enthalten Parser, aber sie können sich im Handling von Edge-Cases und Performance unterscheiden.

Alternativen in Clojure sind:
- `clj-time`, eine Joda-Time Wrapper-Bibliothek, die jedoch nach java.time migriert, da Joda-Time nicht mehr aktiv entwickelt wird.
- `clojure.instant` für einfache Anwendungsfälle.

Implementierungsdetails:
- Die java.time API ist immutable und thread-safe.
- Beim Parsen solltest du dich um Zeitzonen und Locale kümmern, da sie das Parsingergebnis ändern können.

## Siehe Auch
- [Clojure java-time library](https://github.com/dm3/clojure.java-time)
- [clj-time GitHub repository](https://github.com/clj-time/clj-time)
- [Java-Time API Guide](https://docs.oracle.com/javase/tutorial/datetime/index.html)
