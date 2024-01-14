---
title:    "Clojure: Skriving av tester"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester er en viktig praksis i Clojure-programmering. Det hjelper deg å identifisere og fikse feil før de skaper problemer i produksjon, noe som sparer deg for tid og frustrasjon. I tillegg bidrar det til å sikre at koden din er pålitelig og robust.

## Hvordan gjøre det

For å skrive tester i Clojure, bruker vi biblioteket "clojure.test". Først må vi importere det ved å legge til dette i toppen av filen vår:

```Clojure
(require '[clojure.test :refer [deftest is]])
```

Deretter kan du definere en test ved å bruke makroen "deftest". Pass på at navnet på testen følger konvensjonen "test-" + navnet på funksjonen du tester:

```Clojure
(deftest test-addition
  (is (= 6 (+ 2 4)))))
```

I dette eksempelet tester vi funksjonen "addition" og sjekker om resultatet er riktig. For å kjøre testene, bruker vi følgende kommando i REPL: (test-ns 'navn-på-fil). Dette vil kjøre alle testene definert i den angitte filen.

## Dykk dypere

Når du skriver tester, er det viktig å dekke ulike scenarioer og kanttilfeller for å sikre et pålitelig program. Du kan også bruke makroen "is" for å sammenligne verdier og sjekke om de er like. I tillegg kan du bruke forskjellige funksjoner som "throws?" for å teste om en funksjon kaster et forventet unntak.

Det er også viktig å følge god praksis når du skriver tester, som å gjøre testene dine uavhengige av hverandre og å gi dem betegnelser som gjør det enklere å identifisere hva de tester.

## Se også

- [Offisiell Clojure.test-dokumentasjon](https://clojure.github.io/clojure/clojure.test-api.html)
- [Testing i Clojure](https://clojure.org/guides/testing)
- [Eksempel på test-suiten i et Clojure-prosjekt](https://github.com/clojure/clojure/blob/master/test/clojure/test_clojure/test_).clj