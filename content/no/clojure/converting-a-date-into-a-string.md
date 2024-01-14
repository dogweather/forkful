---
title:                "Clojure: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange programmerere vil på et tidspunkt ha behov for å konvertere en dato til en streng, enten for å presentere informasjon for brukere eller lagre den i en database. Å konvertere en dato til en streng gjør det enklere å håndtere og manipulere datoer i programmering.

## Slik gjør du det

```clojure
(let [today (java.time.LocalDate/now)]
  (java.time.format.DateTimeFormatter/ISO_DATE
    .format today))
```

Output: "2021-08-25"

I dette eksempelet bruker vi Clojure-funksjoner for å hente dagens dato og konvertere den til en streng med et ISO format. Du kan også bruke forskjellige formater avhengig av dine behov, som å legge til tidsinformasjon eller endre rekkefølgen på dag, måned og år.

```clojure
(let [today (java.time.LocalDate/now)]
  (java.time.format.DateTimeFormatter
    .ofPattern "dd-MMM-yy"
    .format today))
```

Output: "25-Aug-21"

## Dypdykk

Når du konverterer en dato til en streng, er det viktig å være klar over hvilken tidsone du opererer i. Dette kan påvirke resultatet av konverteringen avhengig av hvilken standard tidsone som brukes. Det er også viktig å konvertere datoer riktig formatert slik at de kan håndteres riktig av programmet ditt.

## Se også

- [Java Time API](https://docs.oracle.com/en/java/javase/16/docs/api/java.base/java/time/package-summary.html)
- [Clojure.org Documentation](https://clojure.org/api/cheatsheet)