---
title:    "Clojure: Skrive tester"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/clojure/writing-tests.md"
---

{{< edit_this_page >}}

##Hvorfor

Å skrive tester (tests) er en viktig del av å lage kvalitetsprogramvare. Ved å skrive tester kan du sikre at koden din fungerer som den skal, og at den ikke bryter når du gjør endringer.

I tillegg kan det også være en god måte å organisere og strukturere koden din på, og bidra til å forbedre din forståelse av hvordan koden fungerer.

##Slik gjør du det

For å skrive tester i Clojure, kan du bruke rammeverket med navn "clojure.test". Dette gir deg funksjonalitet for å definere tester og kjøre dem.

For å lage en test, bruker du funksjonen "deftest". Inne i denne funksjonen kan du definere koden din og den forventede utgangen. For eksempel:

```Clojure
(deftest addition-test
  (is (= (+ 1 1) 2)))

```

I denne testen, bruker vi "is" funksjonen for å sjekke om utgangen av (+ 1 1) er lik 2. Hvis testen feiler, vil den vise en feilmelding med informasjon om hva som faktisk ble beregnet og hva som var forventet.

For å kjøre testene dine, bruk "run-tests" funksjonen. Dette vil vise en oversikt over alle testene som ble kjørt og deres resultater.

##Dypdykk

Det er flere aspekter ved å skrive tester i Clojure som kan være viktige å forstå. For eksempel kan du bruke "def" og "defn" funksjoner for å definere hjelpefunksjoner som kan brukes i testene dine. Du kan også bruke "testing" blokker for å gruppere testene dine.

Det kan også være lurt å lese dokumentasjonen for rammeverket "clojure.test" for å bli bedre kjent med de forskjellige funksjonene som er tilgjengelige for å skrive tester.

##Se også

- [Documentation for clojure.test](https://clojure.github.io/clojure/clojure.test-api.html)
- [An introduction to Clojure testing](https://purelyfunctional.tv/courses/clojure-testing-introduction/)