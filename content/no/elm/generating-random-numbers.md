---
title:                "Generering av tilfeldige tall"
html_title:           "Elm: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Å generere tilfeldige tall i programmering betyr å lage tall som er helt vilkårlige og uforutsigbare. Dette er nyttig for å lage realistiske spill, simuleringer eller til og med for sikkerhetsformål. Programmere gjør det for å skape variasjon, realisme, og øke sikkerheten til sine applikasjoner.

# Hvordan:
For å generere tilfeldige tall i Elm, kan du bruke funksjonen`Random.generate` og spesifisere området for tallene du vil ha. Her er et eksempel på å generere et tilfeldig tall mellom 1 og 10:
```elm
import Random

Random.generate (Random.int 1 10) 
```
Output vil være et tall mellom 1 og 10 hver gang koden kjøres.

Hvis du vil generere et tilfeldig desimaltall, kan du bruke `Random.float` funksjonen og spesifisere området som forventet resultat. For eksempel, for å få et tilfeldig tall mellom 0 og 1 med to desimaler:
```elm
import Random

Random.generate (Random.float 0 1) 
```

# Dykk dypere
Det å generere tilfeldige tall er et vanlig behov i programmering, og har blitt brukt i mange år for ulike formål. Før Elm, var det vanlig å bruke pseudorandom tallgeneratorer, som genererte tall basert på en algoritme og en startverdi. Men dette kan føre til forutsigbare tall og sikkerhetsproblemer. I Elm derimot, brukes en kryptografisk trygg tilnærming for å generere tilfeldige tall, som sikrer at tallene er helt tilfeldige og ikke kan forutsies.

Det finnes også andre alternative metoder for å generere tilfeldige tall i Elm, som å bruke en tredjeparts pakke som tilbyr mer avanserte funksjoner for tilfeldige tall. Men for de fleste tilfeller, er `Random.generate` funksjonen tilstrekkelig.

# Se også
- Elm offisiell dokumentasjon for `Random`
- Elm-samfunnets pakker for tilfeldige tall:
  - [elm-random-pcg](https://package.elm-lang.org/packages/saulecabrera/elm-random-pcg/latest/)
  - [elm-random-extra](https://package.elm-lang.org/packages/gampleman/elm-random-extra/latest/)