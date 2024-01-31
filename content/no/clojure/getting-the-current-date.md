---
title:                "Slik får du tak i dagens dato"
date:                  2024-01-20T15:13:55.066582-07:00
html_title:           "C: Slik får du tak i dagens dato"
simple_title:         "Slik får du tak i dagens dato"

category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Hente nåværende dato i Clojure lar deg merke hendelser og spore endringer over tid. Programmerere bruker det for logging, tidsstyring og funksjonaliteten som krever tidsspesifikke data.

## How to:
I Clojure får du tak i dagens dato slik:

```Clojure
(import 'java.util.Calendar)
(defn current-date []
  (let [cal (Calendar/getInstance)]
    (.getTime cal)))

(println (current-date)) ;; => Sat Mar 25 14:34:56 CET 2023
```

Enklere, bruk `java.time.LocalDate`:

```Clojure
(import 'java.time.LocalDate)
(defn today []
  (str (LocalDate/now)))

(println (today)) ;; => 2023-03-25
```

Output blir dagens dato.

## Deep Dive
Tilbake i Java's tidlige dager brukte vi `java.util.Date`, men det hadde mangler, som datosikkerhet og tidsone-håndtering. `java.util.Calendar` ble introdusert for å gi mer fleksibilitet, men ganske snart kom Java 8 med `java.time` pakken, en sterkere og mer intuitiv API for dato og tid.

I Clojure, en funksjonell LISP-inspirert språk på JVM, er det vanlig å bruke Java-klasser for tid- og dato-operasjoner. Å bruke `java.time.LocalDate/now` er effektivt og gir et ISO-8601 representasjon av datoen, som er et globalt anerkjent format.

Alternativer inkluderer biblioteker som clj-time, et wrapper-bibliotek rundt Joda-Time, men siden Joda-Time nå anbefaler `java.time`, anbefales det kun for eldre prosjekter.

## See Also
- [Clojure's java-time library](https://github.com/dm3/clojure.java-time)
- [Java 8 Date/Time API documentation](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [clj-time GitHub repository](https://github.com/clj-time/clj-time)
