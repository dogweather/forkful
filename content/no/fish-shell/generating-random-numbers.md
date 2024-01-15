---
title:                "Generering av tilfeldige tall"
html_title:           "Fish Shell: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Å generere tilfeldige tall kan være nyttig for en rekke programmeringsoppgaver, for eksempel å skape usikkerhet eller å simulere tilfeldige hendelser. Dette kan være spesielt nyttig for utviklere som jobber med spill, tilfeldige algoritmer eller tester.

## Hvordan

For å generere tilfeldige tall i Fish Shell, kan du bruke kommandoen `math random`. Dette vil gi deg et tilfeldig desimaltall mellom 0 og 1. For å få et heltall, kan du multiplisere tallet med det maksimale ønskede intervallet og runde av resultatet. La oss si at du vil generere et tilfeldig tall mellom 1 og 10, da kan du bruke følgende kommando:

```
Fish Shell:

math random * 10 | round
```

Dette vil gi deg et tilfeldig heltall mellom 1 og 10 som output. Du kan også sette dette i en løkke for å generere flere tilfeldige tall:

```
Fish Shell:
for x in ( seq 5 )
	echo (math random * 10 | round)
end
```

Dette vil gi deg 5 tilfeldige heltall mellom 1 og 10 som output.

## Deep Dive

Fish Shell bruker en pseudorandom generator for å generere tilfeldige tall. Dette betyr at tallene ikke er helt tilfeldige, men genereres gjennom en beregningsprosess som bruker et initialt tall som kalt en "seed". Den genererte sekvensen av tall vil alltid være den samme for en spesifikk seed. Dette betyr at hvis du ønsker å ha en mer tilfeldig følelse, kan du endre seeden ved å bruke kommandoen `math random-set-seed` med et annet tall som argument.

Et annet nyttig aspekt ved å generere tilfeldige tall i Fish Shell er muligheten til å generere tall innenfor et spesifikt intervall. Ved å bruke kommandoen `math random-seed` med et lavere og øvre tall som argumenter, kan du generere tilfeldige tall innenfor dette intervallet. For eksempel, `math random-seed 1 100` vil generere tall mellom 1 og 100.

## Se også

- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/index.html)
- [Tilfeldig tallgenerator i andre programmeringsspråk](https://en.wikipedia.org/wiki/Random_number_generation)