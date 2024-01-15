---
title:                "Generering av tilfeldige tall"
html_title:           "Clojure: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor
Å generere tilfeldige tall er en nøkkelfunksjon i mange programmeringsspråk, og Clojure er intet unntak. Å kunne generere tilfeldige tall åpner døren for et bredt spekter av funksjonaliteter, fra generering av testdata til implementering av algoritmer for simulering og maskinlæring. 

## Hvordan gjøre det
Det er flere måter å generere tilfeldige tall i Clojure, avhengig av behovet ditt.

For å generere et enkelt tilfeldig tall mellom en bestemt start og sluttverdi, kan du bruke ```rand-int``` funksjonen. For eksempel:

```Clojure
(rand-int 10) ;returnerer et tilfeldig tall mellom 0 og 9
(rand-int 50) ;returnerer et tilfeldig tall mellom 0 og 49
``` 

Hvis du trenger et tilfeldig desimaltall, kan du bruke ```rand``` funksjonen. Denne funksjonen returnerer et tall mellom 0 og 1, så du kan multiplisere det med ønsket maksverdi for å få et tilfeldig tall innenfor det ønskede området. For eksempel:

```Clojure
(* 10 (rand)) ;returnerer et tilfeldig tall mellom 0 og 10
(* 50 (rand)) ;returnerer et tilfeldig tall mellom 0 og 50
```

Hvis du vil ha mer kontroll over tilfeldighetsnivået, kan du bruke ```seed-random``` funksjonen for å sette en bestemt frøverdi før du genererer tilfeldige tall. Dette kan være nyttig for å teste kode som bruker tilfeldige tall, siden du kan "sette" en bestemt tilfeldig tallssekvens for å gjenskape resultatene dine. For eksempel:

```Clojure
(seed-random 123)  ;setter frøverdi til 123
(rand) ;returnerer alltid samme tilfeldige tall basert på frøverdien
(rand) ;returnerer alltid samme tilfeldige tall basert på frøverdien
```

## Dykk dypere
Den ```rand-int``` og ```rand``` funksjonene er bare enkle eksempler på hvordan du kan generere tilfeldige tall i Clojure. Hvis du ønsker mer avansert funksjonalitet, kan du se på biblioteker som ```clojure.data.generators``` og ```clojure.test.check```. Disse bibliotekene tilbyr mer omfattende metoder for å generere tilfeldige data og kan hjelpe deg med å bygge mer pålitelige og robuste applikasjoner.

## Se også
- [Offisiell Clojure dokumentasjon for rand-int](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/rand-int)
- [Offisiell Clojure dokumentasjon for rand](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/rand)
- [Clojure data.generators bibliotek](https://clojure.github.io/data.generators/)
- [Clojure test.check bibliotek](https://github.com/clojure/test.check)