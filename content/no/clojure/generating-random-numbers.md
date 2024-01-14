---
title:    "Clojure: Generering av tilfeldige tall"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# Hvorfor

Å generere tilfeldige tall er en viktig del av mange programmeringsoppgaver, særlig når det kommer til å lage tilfeldig oppførsel eller simulere virkelige situasjoner. Det kan også være nyttig for å teste funksjonaliteten til et program eller for å lage tilfeldige datasett for maskinlæring.

# Hvordan

Det er flere måter å generere tilfeldige tall i Clojure på. En av de mest brukte er ved å bruke funksjonen `rand`, som genererer et flytende punkt tall mellom 0 og 1:

```Clojure
(rand)
;; Output: 0.6

(rand)
;; Output: 0.23
```

Hvis vi ønsker å generere et heltall mellom et gitt sett av tall, kan vi bruke `rand-int`-funksjonen:

```Clojure
(rand-int 10)
;; Output: 7

(rand-int 100)
;; Output: 32
```

For å få en tilfeldig valgt verdi fra en liste eller sekvens, kan vi bruke `rand-nth`-funksjonen:

```Clojure
(def fruits ["eple" "banan" "jordbær" "appelsin"])

(rand-nth fruits)
;; Output: "jordbær"
```

Vi kan også generere tilfeldige tall med en gitt fordeling ved å bruke `rand-nth` på en vektor med tall som representerer fordelingsfunksjonen:

```Clojure
(def distribution [0.1 0.2 0.4 0.3])

(rand-nth distribution)
;; Output: 0.4
```

# Dypdykk

Å generere tilfeldige tall basert på en bestemt fordeling kan gjøres ved å bruke `sample`-funksjonen med en liste over tall og en fordelingsvektor:

```Clojure
(sample [1 2 3 4 5 6] distribution)
;; Output: 6
```

Vi kan også bruke `shuffle`-funksjonen til å blande en sekvens tilfeldig:

```Clojure
(shuffle (range 10))
;; Output: (4 8 2 1 7 3 5 0 9 6)
```

For å få en mer pålitelig tilfeldighet kan vi også bruke `seed-random`-funksjonen til å sette en startverdi for tilfeldighetsgeneratoren:

```Clojure
(seed-random 1234)
(rand)
;; Output: 0.6916142

(seed-random 1234)
(rand)
;; Output: 0.6916142
```

# Se også

- [Clojure Docs: Random Numbers](https://clojuredocs.org/clojure.core/rand)
- [Clojure for Java Programmers: Randomness](https://clojure.org/guides/java_interop#_randomness)