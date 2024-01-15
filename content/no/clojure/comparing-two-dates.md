---
title:                "Sammenligner to datoer"
html_title:           "Clojure: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor
Det kan være nyttig å sammenligne to datoer i Clojure for å kunne organisere og filtrere data basert på en gitt tidsperiode. Dette kan være nyttig i en rekke tilfeller, som for eksempel å analysere salgstall over en periode eller planlegge fremtidige aktiviteter basert på tidligere erfaringer.

## Slik gjør du det
```Clojure
(def dato1 (modded-date 2021 2 1))
(def dato2 (modded-date 2021 3 1))

(println "Er dato1 før dato2? " (< dato1 dato2))
(println "Er dato1 etter dato2? " (> dato1 dato2))
(println "Er dato1 og dato2 like? " (= dato1 dato2))
```
Output:
```
Er dato1 før dato2? true
Er dato1 etter dato2? false
Er dato1 og dato2 like? false
```

## Dypdykk
I Clojure er det flere funksjoner som kan brukes for å sammenligne datoer. En vanlig metode er å konvertere datoene til et numerisk format, for eksempel antall sekunder siden starten av Unix-tiden. Dette gjør det enklere å sammenligne datoer ved hjelp av de innebygde sammenligningsfunksjonene (<, >, =).

En annen metode er å bruke funksjonen `compare`, som returnerer en negativ, positiv eller null verdi avhengig av om den første datoen er mindre enn, større enn eller lik den andre datoen. Dette gjør det enklere å bruke datoer som nøkler i et map eller sorteringsfunksjoner.

## Se også
- [ClojureDocs: Comparing Dates](https://clojuredocs.org/clojure.core/compare)
- [ClojureDocs: Date and Time Functions](https://clojuredocs.org/clojure.java-time)
- [Clojure Cookbook: Comparing Dates and Times](https://clojure-cookbook.net/dates-and-times/comparing)