---
title:                "Få den gjeldende datoen"
html_title:           "Haskell: Få den gjeldende datoen"
simple_title:         "Få den gjeldende datoen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å hente gjeldende dato handler om å få programmet ditt til å forstå dagens dato i virkelige verden. Dette er nyttig for programmerere til blant annet logging, tidspunktstamping og tidsavhengige operasjoner som aftaler eller påminnelser.

## Hvordan:

Clojure gir lett tilgang til funksjoner som kan hente dagens dato. En enkel måte å få det på er å bruke `java.util.Date` klassen.

```Clojure
(import 'java.util.Date)

(defn current-date []
  (Date.))

(println (current-date))
```

Når dette programmet kjøres, vil det skrive ut dagens dato og klokkeslett, for eksempel:

```Clojure
Tue Sep 07 17:20:56 CEST 2021
```

## Dyp Dykk

Å hente dagens dato i programmering har en lang historie og mange varianter avhengig av spesifikke use-case og tidssonebehov. Noen programmerere kan foretrekke å bruke `java.time.LocalDate`, som gir LocalDate-objektet som representerer dagens dato uten tidspunkt. Det kan være mer hensiktsmessig for noen applikasjoner. For eksempel:

```Clojure
(import 'java.time.LocalDate)

(defn current-date []
  (.now LocalDate))

(println (current-date))
```

Dette vil gi noe som ligner: `2021-09-07`.

Clojure implementerer disse funksjonene ved å bruke Java Platform, Standard Edition (Java SE) API-et, som gir robuste tids- og datofunksjoner.

## Se Også

For mer detaljerte instruksjoner og eksempler på dato- og tidsfunksjoner i Clojure, besøk disse linkene:

- [Clojure official documentation](https://clojure.org/guides/getting_started)
- [Java Platform Standard Edition 8 API Specification](https://docs.oracle.com/javase/8/docs/api/)
- [Working with Dates and Times in Clojure](https://www.baeldung.com/java-dates-clojure)