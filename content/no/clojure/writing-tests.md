---
title:                "Clojure: Skriver tester"
simple_title:         "Skriver tester"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Testdrevet utvikling (TDD) er en utviklingsmetodikk som fokuserer på å skrive tester før selve implementeringen av koden. Dette kan virke motintuitivt, men det bidrar til å forbedre kvaliteten på koden og øker sannsynligheten for at koden fungerer som den skal.

## Hvordan

```Clojure
(defn add [a b]
  (+ a b))
```

Dette er et enkelt eksempel på en funksjon som legger sammen to tall i Clojure. For å skrive en test for denne funksjonen, kan vi bruke biblioteket `clojure.test` og definere en testfunksjon som sjekker om funksjonen returnerer riktig resultat:

```Clojure
(require '[clojure.test :refer :all])

(deftest test-addition
  (is (= 3 (add 1 2))))
```

Når vi kjører denne testen, vil vi se at den passerer. Men hva skjer hvis vi endrer funksjonen slik at den returnerer feil resultat?

```Clojure
(defn add [a b]
  (- a b))
```

Nå vil testen feile, og vi må rette opp i feilen før vi kan fortsette å utvikle koden vår. Dette viser hvorfor det er viktig å skrive tester før implementeringen av koden, slik at vi kan fange eventuelle feil tidlig og unngå å introdusere feil senere i utviklingsprosessen.

## Dypdykk

Å skrive gode tester kan ta tid og kreve mye tankearbeid, men det er vel verdt innsatsen. Det er viktig å tenke på kanttilfellene og sikre at alle mulige scenarioer er dekket av testene våre. Det kan også være lurt å bruke funksjonelle konstruksjoner som for eksempel generative testing for å øke testdekningen vår.

Det kan også være en god idé å følge prinsipper som for eksempel "Arrange, Act, Assert" og "One Assertion Per Test" for å skrive mer strukturerte og lesbare tester.

## Se også

- [Clojure.test dokumentasjon](https://clojure.github.io/clojure/clojure.test-api.html)
- [Clojure for the Brave and True](https://www.braveclojure.com/testing/)
- [The Rationale Behind Test-Driven Development](https://www.thoughtworks.com/insights/blog/rationale-behind-test-driven-development)