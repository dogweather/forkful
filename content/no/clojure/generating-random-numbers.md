---
title:    "Clojure: Generering av tilfeldige tall"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Å generere tilfeldige tall er essensielt for å skape variasjon og tilfeldighet i koden din. Dette kan være nyttig for alt fra å lage spill til å utføre simulasjoner.

## Hvordan

For å generere tilfeldige tall i Clojure, kan du bruke funksjonen `rand`. Denne funksjonen tar inn et tall som argument, og vil gi tilbake et tilfeldig tall mellom 0 og det spesifiserte tallet. For eksempel:

```Clojure
(rand 10)  ; Vil returnere et tilfeldig tall mellom 0 og 10
```

Du kan også bruke funksjonen `random` for å generere et tilfeldig tall mellom to spesifiserte tall, som vist i eksempelet under:

```Clojure
(random 5 10)  ; Vil returnere et tilfeldig tall mellom 5 og 10
```

En annen nyttig funksjon er `shuffle`, som tar inn en liste og gir tilbake en tilfeldig permutasjon (ombytning av elementer) av den opprinnelige listen. Dette kan være nyttig for å blande rekkefølgen på elementer i en liste.

```Clojure
(shuffle [1 2 3 4 5])  ; Et eksempel på en tilfeldig permutasjon av tallene 1 til 5
```

## Dypdykk

Det er viktig å være klar over at tilfeldige tall generert ved hjelp av disse funksjonene ikke er helt tilfeldige. De er basert på en såkalt pseudorandom-algoritme, som bruker en startverdi (også kalt "seed") for å generere tallene. Dette betyr at hvis du angir samme startverdi, vil du alltid få de samme tallene i samme rekkefølge.

For å unngå dette og få en virkelig tilfeldig liste av tall, kan du bruke funksjonen `java.util.Random` og deretter bruke `nextInt`-funksjonen for å generere tilfeldige tall. For å sikre at du alltid får forskjellige tall, bør du bruke en variabel for å lagre `Random`-objektet og kalle `setSeed`-funksjonen med forskjellige verdier mellom hver kjøring av programmet.

```Clojure
(def rnd (java.util.Random.))  ; Oppretter et tilfeldig-objekt
(setSeed rnd 42)  ; Setter startverdi til 42
(.nextInt rnd 10)  ; Vil gi tilbake et helt tilfeldig tall mellom 0 og 9
```

## Se også

- [Clojure dokumentasjon for `rand`](https://clojuredocs.org/clojure.core/rand)
- [Clojure dokumentasjon for `random`](https://clojuredocs.org/clojure.core/random)
- [Clojure dokumentasjon for `shuffle`](https://clojuredocs.org/clojure.core/shuffle)
- [Java dokumentasjon for `Random`](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/Random.html)