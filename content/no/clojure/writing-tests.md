---
title:                "Skriver tester"
html_title:           "Clojure: Skriver tester"
simple_title:         "Skriver tester"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester i Clojure er en viktig del av å sikre at koden din fungerer som den skal. Det hjelper deg med å oppdage feil og problemer tidlig i utviklingsprosessen, noe som sparer deg for mye tid og frustrasjon senere.

## Hvordan gjøre det

For å skrive tester i Clojure, bruker du et testrammeverk som heter `clojure.test`. La oss se på et eksempel på hvordan du kan skrive en enkel test som sjekker om en funksjon returnerer riktig verdi.

```Clojure
(use 'clojure.test)

(defn add [x y]
  (+ x y))

(deftest test-add
  (is (= 4 (add 2 2))))
```

I dette eksempelet definerer vi en funksjon `add` som tar to tall og returnerer summen av dem. Deretter definerer vi en test med navnet `test-add` hvor vi bruker funksjonen `is` for å sjekke om verdien av `add` med argumentene 2 og 2 er lik 4. For å kjøre denne testen, kan du bruke kommandoen `lein test`.

```Clojure
lein test

Ran 1 tests containing 1 assertions.
0 failures, 0 errors.
```

Du vil se at testen har passert, noe som betyr at vår `add` funksjon fungerer som den skal. La oss nå se på et eksempel der testen feiler.

```Clojure
(use 'clojure.test)

(defn divide [x y]
  (/ x y))

(deftest test-divide
  (is (= 2 (divide 10 0))))
```

Nå har vi definert en funksjon `divide` som deler et tall på et annet tall. Denne gangen definerer vi en test som forventer at når vi deler 10 på 0, skal resultatet være 2. Når vi kjører testen, vil den feile og gi følgende output.

```Clojure
lein test

FAIL in (test-divide) (form-init3535005991415619436.clj:241)
expected: (= 2 (divide 10 0))
  actual: (not (= 2 Infinity))
```

Testen feilet fordi det er umulig å dele på 0, og derfor returnerer funksjonen `divide` "Infinity". Dette viser hvor viktig det er å skrive tester, slik at du kan fange slike feil tidlig i utviklingsprosessen.

## Dypdykk

Clojure har en rekke funksjoner og verktøy som du kan bruke i tester for å gjøre dem mer robuste og fleksible. En av disse er funksjonen `testing`, som lar deg gruppere relaterte tester sammen. Du kan også bruke `are` funksjonen til å teste en funksjon med flere forskjellige argumenter samtidig.

En annen nyttig funksjon er `deftest*`, som lar deg definere mer komplekse tester med tilleggsfunksjoner som tar imot parametere og returnerer verdier. Dette er spesielt nyttig når du tester funksjoner som tar imot argumenter av forskjellige typer.

## Se også

- [Clojure Docs](https://clojure.org/guides/getting_started)
- [Clojure test rammeverk](https://clojure.github.io/clojure/clojure.test-api.html)