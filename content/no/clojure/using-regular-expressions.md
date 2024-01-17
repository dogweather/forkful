---
title:                "Å bruke regulære uttrykk"
html_title:           "Clojure: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Bruk av regulære uttrykk er en måte for programmerere å matche og manipulere tekststrenger på en fleksibel måte. Dette er spesielt nyttig når man ønsker å finne bestemte mønstre i en tekst, for eksempel å validere en e-postadresse eller finne alle ord som starter med en bestemt bokstav. Regulære uttrykk brukes ofte for å automatisere komplekse oppgaver, og kan spare mye tid og arbeid for programmerere.

## Hvordan:

For å bruke regulære uttrykk i Clojure, kan du bruke funksjonen `re-find` eller `re-seq`. Her er et eksempel på hvordan du kan bruke regulære uttrykk for å finne alle ord som begynner med bokstaven "a" i en tekststreng:

```
(def tekst "Hei på deg, alle sammen!")

(re-find #"a\w+" tekst)
```

Dette vil returnere en liste med alle ord som starter med "a" i teksten, i dette tilfellet "alle" og "alene".

## Dykk dypere:
Regulære uttrykk har vært en del av programmering siden 1950-tallet og er fortsatt et kraftig verktøy for å manipulere tekst. Alternativene til regulære uttrykk inkluderer string-manipuleringsfunksjoner, men disse kan ofte være mindre effektive og mer komplekse å implementere. Implementasjonen av regulære uttrykk i Clojure er basert på Java's `java.util.regex` bibliotek, og støtter derfor også alle vanlige regulære uttrykk-mønster og -funksjoner.

## Se også:
- [Clojure.org - Regular Expressions](https://clojure.org/guides/regular_expressions)
- [Mastering Clojure Regex](https://www.braveclojure.com/regular-expressions/)
- [Javadoc: java.util.regex](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/regex/package-summary.html)