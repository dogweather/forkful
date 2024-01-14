---
title:    "Clojure: Å gjøre en streng stor"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Hvorfor
Mange programmeringsoppgaver krever at man må endre strenger (en serie med bokstaver eller tall) for å få ønsket resultat. En av de vanligste operasjonene man kan gjøre med en streng er å gjøre den større eller mindre bokstaver. Dette kan være nyttig for å vise data på en konsistent måte eller for å søke og filtrere data mer effektivt.

## Hvordan
For å endre strenger til store eller små bokstaver, kan man bruke Clojure-funksjonen `clojure.string/capitalize`. Denne funksjonen tar inn en streng som argument og returnerer en ny streng med første bokstav oppkappitalisert. Her er et eksempel på hvordan man kan bruke denne funksjonen:

```Clojure
(clojure.string/capitalize "clojure er et fantastisk programmeringsspråk")
```
Output: "Clojure er et fantastisk programmeringsspråk"

Man kan også bruke funksjonen `upper-case` for å endre alle bokstaver i en streng til store bokstaver:

```Clojure
(upper-case "clojure")
```
Output: "CLOJURE"

## Dypdykk
Det finnes også andre metoder for å endre størrelsen på bokstaver i en streng, som for eksempel `lower-case` for å gjøre alle bokstavene små eller `capitalize-words` for å gjøre første bokstav i hvert ord i en streng stor. Clojure har også funksjoner for å bytte ut enkeltbokstaver eller sette alle bokstavene i en streng i motsatt rekkefølge. Det er viktig å merke seg at disse funksjonene ikke endrer den opprinnelige strengen, men returnerer en ny endret streng.

## Se også
- [Clojure dokumentasjon](https://clojure.org/api/cheatsheet)
- [Eksempler på string-manipulasjon i Clojure](https://rosettacode.org/wiki/Category:Clojure)