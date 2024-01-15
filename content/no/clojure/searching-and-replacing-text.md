---
title:                "Søking og erstatter av tekst"
html_title:           "Clojure: Søking og erstatter av tekst"
simple_title:         "Søking og erstatter av tekst"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du skriver mye kode, vil du kanskje finne deg selv i en situasjon der du trenger å erstatte en del av teksten med en annen del. Dette kan være for å rette opp en feil eller for å gjøre en endring i koden din. I slike tilfeller kan det være nyttig å kunne automatisere denne prosessen ved hjelp av Clojure.

## Hvordan

For å erstatte tekst i Clojure, kan du bruke funksjonen `replace` i `clojure.string` biblioteket. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```Clojure
(require '[clojure.string :as string])

(def tekst "Dette er en test tekst.")

(string/replace tekst "test" "eksempel")
```

Dette vil returnere en ny streng med `tekst` erstattet med `eksempel`. Output vil være:

```Clojure
"Dette er en eksempel tekst."
```

Du kan også bruke `replace` funksjonen til å erstatte deler av en streng basert på et mønster. For eksempel kan du erstatte alle forekomster av bokstaven `e` med `a` ved å bruke et regex mønster:

```Clojure
(require '[clojure.string :as string])

(def tekst "Dette er en test tekst.")

(string/replace tekst #"e" "a")
```

Output vil da være:

```Clojure
"Datta ar an tast ta akt."
```

## Dypdykk

I tillegg til å kunne erstatte tekst, kan du også bruke Clojure til å finne og erstatte tekst basert på bestemte kriterier. Dette kan gjøres ved hjelp av funksjonene `find` og `replace` i `clojure.string` biblioteket.

For å finne alle forekomster av `test` i en streng, kan du bruke `find` funksjonen:

```Clojure
(require '[clojure.string :as string])

(def tekst "Dette er en test tekst.")

(string/find tekst #"test")
```

Dette vil returnere en liste med alle forekomster av `test`. Du kan deretter bruke denne listen til å erstatte deler av teksten ved hjelp av `replace` funksjonen.

## Se også

- [Clojure dokumentasjon](https://clojure.org/)
- [Clojure Cookbook](https://clojure-cookbook.com/)