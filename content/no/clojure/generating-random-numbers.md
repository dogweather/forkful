---
title:                "Clojure: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor?

Det å generere tilfeldige tall er et viktig verktøy for mange utviklere i ulike programmeringsspråk, inkludert Clojure. Det kan brukes til å lage realistiske datasett, teste koden din eller bare legge til en tilfeldig komponent i et annet program. Så hvorfor skulle du ønske å lære å generere tilfeldige tall i Clojure?

## Hvordan?

Å generere tilfeldige tall i Clojure er enkelt og krever bare noen få linjer med kode. Først må du importere funksjonene fra Clojure's `clojure.math.numeric-tower` bibliotek:

```Clojure
(require '[clojure.math.numeric-tower :as math])
```

Deretter kan du bruke funksjonen `rand` for å generere et tilfeldig desimaltall mellom 0 og 1:

```Clojure
(math/rand)
```

For å generere et heltall, kan du bruke funksjonene `rand-int` eller `rand-nth`:

```Clojure
(math/rand-int 10) ;; genererer et tilfeldig heltall mellom 0 og 9
(math/rand-nth [1 2 3 4 5]) ;; returnerer et tilfeldig element fra listen
```

Du kan også begrense området for tallene ved å bruke funksjonen `rand-int` med et argument:

```Clojure
(math/rand-int 50) ;; genererer et tilfeldig heltall mellom 0 og 49
```

## Dypdykk

Det som skjer bak kulissene når du bruker funksjonen `rand` er at den egentlig kaller på en Java-funksjon som returnerer en tilfeldig verdi basert på en såkalt "seed" (utgangspunkt). En seed er en startverdi som brukes til å generere et tilfeldig tall, og i Clojure er denne verdien basert på klokkeslettet. Dette betyr at du vil få en annen tilfeldig verdi hver gang du kjører koden din.

For mer avanserte tilfeldige tall generatorer i Clojure, kan du også sjekke ut biblioteket `core.matrix` som tilbyr flere ulike typer tilfeldige tall, som for eksempel Gaussiske distribusjoner og normalfordelte tall.

## Se også

- [Clojure Dokumentasjon: Tilfeldige tall](https://clojuredocs.org/clojure.math.numeric-tower/rand-int)
- [The Joy of Clojure: Generer tilfeldige tall](https://thejoyofclojure.com/generer-tilfeldige-tall.html)
- [Clojure Cookbook: Generer tilfeldige tall](https://clojure-cookbook.com/math/random.html)