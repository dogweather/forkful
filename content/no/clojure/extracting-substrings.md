---
title:                "Ekstrahering av delstrenger"
html_title:           "Clojure: Ekstrahering av delstrenger"
simple_title:         "Ekstrahering av delstrenger"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Utvinning av substrings er en verdifull verktøy i Clojure for å manipulere og bearbeide tekst. Dette kan være nyttig for å håndtere brukerinput, analysere data og mer.

## Hvordan

Utvinning av substrings i Clojure gjøres ved å bruke funksjonen `subs`. Denne funksjonen tar tre argumenter: en streng, startindeks og sluttpindeks, og returnerer en ny streng som inneholder en del av den opprinnelige strengen fra den angitte startindeksen til sluttpindeksen.

```Clojure
(println "Velkommen til Clojure!")     ; output => Velkommen til Clojure!
(println (subs "Velkommen til Clojure!" 10))     ; output => til Clojure!
(println (subs "Velkommen til Clojure!" 4 11))     ; output => kommen
```

Den første koden viser hvordan `println` fungerer for å skrive ut en streng. Deretter bruker vi `subs` for å utvinne substring i strengen fra det 10. tegnet og ut. Dette resulterer i at de første 9 tegnene blir utelatt fra strengen som skrives ut. I den siste koden utvinner vi substringen fra det 4. tegnet til det 11. tegnet, som er "kommen".

Du kan også bruke funksjonen `subs` for å utvinne substrings fra slutten av en streng ved å bruke et negativt tall for sluttpindeksen. Dette vil utvinne substrings fra slutten av strengen opp til, men ikke inkludert, det angitte negative tallet.

```Clojure
(println (subs "Velkommen til Clojure!" -7))     ; output => Clojure!
(println (subs "Velkommen til Clojure!" 4 -5))     ; output => kommen til
```

I den første koden, bruker vi et negativt tall for sluttpindeksen for å utvinne substringen fra de siste 7 tegnene i strengen. I den andre koden utvinnes substringen fra det 4. tegnet til de siste 5 tegnene i strengen.

## Dypdykk

I tillegg til startindeks og sluttpindeks, kan funksjonen `subs` også ta et tredje argument for å angi steg. Dette lar deg utvinne en del av en streng med et spesifikt intervall mellom hvert tegn.

```Clojure
(println (subs "Velkommen til Clojure!" 0 7 2))     ; output => Vlmmn
(println (subs "Velkommen til Clojure!" 1 10 3))     ; output => eklo
```

I den første koden bruker vi et steg på 2, som resulterer i at bare hvert andre tegn blir inkludert i substringen. I den andre koden bruker vi et steg på 3, og resulterer i at bare hvert tredje tegn blir inkludert i substringen. Dette er nyttig hvis du for eksempel bare trenger å utvinne deler av en streng som er i et bestemt mønster.

## Se Også

- [Clojure dokumentasjon for `subs` funksjonen](https://clojuredocs.org/clojure.core/subs)
- [En tutorial om å utvinne substrings i Clojure](https://purelyfunctional.tv/lesson/extracting-subs/)