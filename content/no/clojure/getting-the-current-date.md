---
title:    "Clojure: Å få dagens dato"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# Hvorfor

I dagens samfunn er informasjon om datoen viktig. Enten det er for å planlegge jobb eller personlige aktiviteter, vil å vite den korrekte datoen være nyttig. I denne blogginnlegget skal vi se på hvordan du enkelt kan få tak i den nåværende datoen ved hjelp av Clojure-programmering.

# Hvordan få tak i dagens dato

Det er flere måter å få tak i dagens dato på i Clojure. La oss starte med en enkel løsning som bruker standardbiblioteket.

```Clojure
; Importer biblioteket
(ns my-app
  (:require [clojure.java.time :as time]))

; Bruk time-funksjonen til å få tak i dagens dato
(time/today)
```

Dette vil gi oss utdataen som et `java.time.LocalDate`-objekt, som representerer datoen. Du kan også få tak i dagens dato som en streng ved å bruke `format`-funksjonen.

```Clojure
(time/format (time/today) "yyyy-MM-dd")
; Utdata: "2020-10-27"
```

Vi kan også få mer spesifikk informasjon om datoen ved å bruke ulike funksjoner fra biblioteket. For eksempel kan vi få tak i ukedagen og måneden ved hjelp av `day-of-week` og `month`-funksjonene.

```Clojure
(time/day-of-week (time/today))
; Utdata: TUESDAY

(time/month (time/today))
; Utdata: :OCTOBER
```

# Dypdykk

Hvis du ønsker å forstå mer om hvordan tid håndteres i Clojure, kan vi se på [java.time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html) biblioteket som Clojure bruker. Dette biblioteket inneholder en rekke nyttige klasser og funksjoner for å håndtere dato og tid i Java.

Clojure har også et eget bibliotek som kan hjelpe deg med å håndtere dato og tid, kalt [java-time](https://cljdoc.org/d/java-time/clojure.java-time/1.0.0-alpha2/api/clojure.java-time). Dette biblioteket er basert på java.time, men gjør det enklere å bruke i Clojure ved å tilby navnekonverteringer og andre Nyttige funksjoner.

# Se også

Her er noen nyttige ressurser for å lære mer om dato og tid i Clojure:

- [Clojure-biblioteksiden for java-time](https://cljdoc.org/d/java-time/clojure.java-time/1.0.0-alpha2)
- [Offisiell dokumentasjon for java.time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)