---
title:                "Generere tilfeldige tall"
html_title:           "Arduino: Generere tilfeldige tall"
simple_title:         "Generere tilfeldige tall"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Generere tilfeldige tall i Clojure

## Hva & Hvorfor?

Generere tilfeldige tall er å opprette numeriske verdier på en uforutsigbar måte. Programmerere gjør dette for å simulere forskjellige scenarioer, lage testdata, eller la spill ha uforutsigbare utfall.

## Hvordan:

Her er et enkelt eksempel på hvordan du genererer alle slags tilfeldige tall i Clojure:

```Clojure
(defn tilfeldig-tall []
 (rand-int 100))
```

Eksempel output:

```Clojure
(tilfeldig-tall) ;; Kjører denne funksjonen kan gi forskjellige resultater hver gang, for eksempel 37, 72, osv.
```

En annen måte:

```Clojure
(defn tilfeldig-tall-innen-område [min max]
  (+ min (rand-int (- max min))))
```

Eksempel output:

```Clojure
(tilfeldig-tall-innen-område 50 60) ;; gir tall mellom 50 og 60 som 52, 57, osv.
```

## Dyp Dykk

Clojure's `rand` og `rand-int` funksjoner er basert på Java's `java.util.Random` klasse. Dette betyr at uforutsigbarheten og ytelsen til disse funksjonene er i stor grad avhengig av Java's tilsvarende funksjoner.

I historisk kontekst, den første algoritmen for generering av pseudo-tilfeldige tall ble foreslått av en britisk matematiker, John von Neumann, i 1946. Commits til Clojure's kodebase antyder at dens innebygde funksjoner for generering av tilfeldige tall har vært tilstede siden den aller første utgivelsen.

Alternativer til `rand` og `rand-int` kan være algoritmer som Mersenne Twister, Xoshiro256**, eller PCG. Disse gir bedre statiske egenskaper eller ytelse, men kan være vanskeligere å implementere. 

## Se Også

Du kan sjekke ut de følgende koblingene for mer informasjon:
- Clojure's offisielle dokumentasjon for `rand` og `rand-int`: [Clojure Doc](https://clojure.org/reference/data_structures#Numbers)
- Java's `Random` klasse dokumentasjon: [Java Doc](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)