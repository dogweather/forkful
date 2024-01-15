---
title:                "Konvertering av dato til streng"
html_title:           "Clojure: Konvertering av dato til streng"
simple_title:         "Konvertering av dato til streng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan være mange grunner til å ønske å konvertere en dato til en streng i Clojure. Noen ganger må man formatere datoen i en spesifikk måte for å kunne vise den i et brukergrensesnitt eller lagre den i en database. Andre ganger ønsker man å sammenligne datoer eller gjøre beregninger basert på datoen. Uansett hva årsaken er, er det nyttig å vite hvordan man kan konvertere en dato til en streng i Clojure.

## Slik gjør du det

Konvertering av en dato til en streng i Clojure er enkelt med hjelp av funksjonen `format` i biblioteket `clojure.core`. Denne funksjonen tar inn en streng som beskriver ønsket format for datoen, og en dato som skal formateres. Her er et eksempel på hvordan man kan konvertere dagens dato til en streng med formatet "dd/MM/yyyy".

```Clojure
(format "dd/MM/yyyy" (java.util.Date.))
```
Dette vil returnere en streng som ser slik ut: "14/06/2021".

Man kan også spesifisere andre format, som for eksempel "yyyy-MM-dd", som vil returnere datoen i formatet "2021-06-14". Her er en liste med noen av de vanligste formatene:

- dd/MM/yyyy: Dato i formatet dag/måned/år
- MM/dd/yyyy: Dato i formatet måned/dag/år
- dd-MM-yyyy: Dato i formatet dag-måned-år
- yyyy-MM-dd: Dato i formatet år-måned-dag

Det er også mulig å formatere datoen basert på formatet til landet man befinner seg i. For eksempel vil formatet "dd-MMM-yyyy" returnere datoen i formatet "14-Jun-2021" hvis man befinner seg i Storbritannia.

## Dypdykk

I tillegg til de grunnleggende formatene som er nevnt over, kan man også formatere datoen med spesifikke tilleggsparametere. For eksempel, hvis man ønsker å legge til timer og minutter i datoen, kan man bruke formatet "dd/MM/yyyy HH:mm". Dette vil returnere noe slikt som "14/06/2021 13:30". Mer informasjon om alle disse formatene kan finnes i Clojure-dokumentasjonen for `format`.

Å gjøre seg kjent med hvordan man formaterer datoer i Clojure kan være svært nyttig når man arbeider med forskjellige typer data, spesielt når man jobber med oppgaver som krever behandling og manipulering av datoer.

## Se også

- [Offisiell Clojure Dokumentasjon for Datoformat](https://clojuredocs.org/clojure.core/format)