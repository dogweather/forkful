---
title:                "Clojure: Skriving av tester"
programming_language: "Clojure"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester er en viktig del av enhver programmerers verktøykasse. Det hjelper deg å oppdage feil tidligere, sikre at koden din fungerer som forventet og bidra til å opprettholde en sunn kodebase. Det er også en god praksis å skrive tester i et språk som Clojure, da det gjør det enklere å isolere og teste funksjonene dine.

## Hvordan

For å skrive tester i Clojure, kan du bruke biblioteket ```clojure.test```. Først må du importere det i din Clojure-fil med ```(:require [clojure.test :refer :all])```. Deretter kan du definere en test ved å bruke makroen ```deftest```. For eksempel:

```Clojure
(deftest addition-test
  (is (= (+ 2 2) 4)))
```

Dette definerer en test med navnet "addition-test" som bruker funksjonen ```is``` til å sammenligne resultatet av ```(+ 2 2)``` med 4. For å kjøre denne testen, kan du bruke makroen ```run-tests```:

```Clojure
(run-tests)
```

Dette vil kjøre alle testene i filen din og gi deg resultatet. Hvis testen din lykkes, vil du se en melding som sier "Ran 1 test. All tests passed." Hvis testen din feiler, vil du få en feilmelding som gir deg informasjon om hvorfor testen ikke passerte.

## Dypdykk

Når du skriver tester, er det viktig å tenke på kanttilfeller. For eksempel kan du teste hva som skjer når du gir funksjonen din ugyldig input, eller når du prøver å dele på null. Dette vil bidra til å sikre at koden din håndterer disse tilfellene på en riktig måte.

Du kan også bruke funksjonen ```testing``` for å gruppere relaterte tester sammen. Dette gjør det enklere å organisere og lese testkoden din.

Clojure har også andre nyttige testbiblioteker som ```clojure.test.check``` og ```midje```. Det kan være lurt å undersøke disse for å se om de passer bedre til dine spesifikke behov.

## Se også
- [Clojure Dokumentasjon for test](https://clojure.org/guides/learn/testing)
- [En guide til Clojure test](https://www.vikingcodeschool.com/clojure/testing-clojure-code)
- [Clojure.test API-dokumentasjon](https://clojuredocs.org/clojure.test)