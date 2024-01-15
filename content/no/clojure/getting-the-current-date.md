---
title:                "Få nåværende dato"
html_title:           "Clojure: Få nåværende dato"
simple_title:         "Få nåværende dato"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor
Det er ofte nyttig å vite den nåværende datoen for å kunne utføre ulike oppgaver, som for eksempel å lagre økonomiske transaksjoner eller planlegge framtidige hendelser. I Clojure kan du enkelt få tak i den nåværende datoen ved å bruke innebygde funksjoner.

## Slik gjør du det
```Clojure
(def today (java.util.Date.))
```
I dette eksempelet bruker vi funksjonen `java.util.Date` for å lage et Java Date-objekt og lagrer det i variabelen `today`. Dette vil tilsvare den nåværende datoen og tiden på datamaskinen din. Vi kan også bruke funksjonen `instant` fra `java.time` biblioteket for å få nøyaktig samme resultat.

```Clojure
(require '[java.time :as time])

(def today (time/instant))
```

Det er også mulig å få tak i den nåværende datoen i et spesifikt tidsformat ved hjelp av funksjonen `formatter` fra `java.time.format` biblioteket.

```Clojure
(require '[java.time.format :as fmt])

(def today (java.time.LocalDateTime/now))
(def formatted-date (fmt/formatter "dd-MM-yyyy").format(today))
```

## Dypdykk
Hvis vi dykker dypere ned i koden, kan vi se at begge eksemplene ovenfor bruker Java-klassen `Date` for å få tak i den nåværende datoen. Denne klassen er en del av Java API og gir oss tilgang til funksjoner for å håndtere dato- og tidsrelaterte operasjoner. Ved å bruke Clojure sine automatisk konverteringer mellom Java- og Clojure-datastrukturer, kan vi enkelt få tilgang til disse funksjonene.

## Se også
- [Clojure dokumentasjon](https://clojure.org/about/documentation)
- [Java API dokumentasjon for Date-klassen](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Java.time API dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)