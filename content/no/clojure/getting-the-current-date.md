---
title:                "Clojure: Få dagens dato"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er ofte nyttig å ha tilgang til nåværende dato i programmering. Dette kan være nyttig for å lage tidsstemplinger, oppdatere databaser, eller generelt brukes til å håndtere informasjon som er datostyrt. I Clojure, er det enkelt å få tak i nåværende dato ved hjelp av innebygde funksjoner og biblioteker.

## Hvordan

Først må vi importere `java.util.Date` biblioteket, som lar oss arbeide med dato og tid.

```Clojure
(ns minprosjekt.core
    (:import [java.util Date]))
```

Deretter kan vi bruke funksjonen `new` fra `Date` biblioteket for å få tak i nåværende dato og lagre det i en variabel.

```Clojure
(def nåværende-dato (new Date))
```

Vi kan også bruke biblioteket `date-time` fra Clojures standardbibliotek for å få en mer leselig datoformat.

```Clojure
(ns minprosjekt.core
    (:require [clojure.java-time :as time]))

(def nåværende-dato (time/now))
```

Dette vil gi oss en dato i ISO 8601 format, for eksempel `#inst "2021-08-12T14:00:00.000-00:00"`. Hvis vi ønsker å konvertere dette til en mer leselig streng, kan vi bruke funksjonen `formatter` fra samme bibliotek.

```Clojure
(ns minprosjekt.core
    (:require [clojure.java-time :as time]))

(def nåværende-dato (time/now))
(def formatering (time/formatter "dd MMM yyyy"))
(str (formatering nåværende-dato))
```

Dette vil gi oss en streng som ser slik ut: `12 Aug 2021`.

## Deep Dive

Den nåværende datoen er representert av en `java.util.Date`-objekt i Clojure. Hvis vi ønsker å jobbe mer detaljert med datoen, for eksempel endre tidszonen eller justere for forskjeller mellom Gregorian og Julian kalender, må vi bruke Java's `java.time` bibliotek.

Vi kan gjøre dette ved å konvertere vårt `java.util.Date`-objekt til et `java.time.Instant`-objekt, som representerer en punkt i tid. Deretter kan vi jobbe med dette objektet ved hjelp av `java.time` funksjoner.

```Clojure
(ns minprosjekt.core
    (:require [java.time :as time]))

(def nåværende-tidspunkt (time/Instant/ofEpochMilli (.getTime nåværende-dato)))
(def sone (time/ZoneId/of "Europe/Oslo"))
(def justert-tid (time/ZonedDateTime/ofInstant nåværende-tidspunkt sone))
```

Her har vi konvertert vår nåværende dato til et `java.time.ZonedDateTime`-objekt, som representerer en dato og tid med en spesifikk tidsone. Vi kan nå bruke denne til å hente mer informasjon, som for eksempel dag i uken, time, minutt, osv.

## Se også

- https://clojure.org/api/java.time
- https://clojure.org/guides/java_time
- https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html