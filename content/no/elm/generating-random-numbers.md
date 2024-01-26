---
title:                "Generering av tilfeldige tall"
date:                  2024-01-20T17:49:12.259947-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generering av tilfeldige tall"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Å generere tilfeldige tall betyr å lage et tall som ikke kan forutses logisk. Programmerere bruker dette for spill, simuleringer og sikkerhet, for å bringe inn elementet av tilfeldighet.

## How to:
Random-modulen i Elm lar deg generere tilfeldige tall. Her er et enkelt eksempel på hvordan du bruker den:

```elm
import Random

-- Initialiser en generator
randomGenerator : Random.Generator Int
randomGenerator = Random.int 1 100

-- For å faktisk generere et tall, bruker vi en kommando
generateRandomNumber : Cmd msg
generateRandomNumber = Random.generate MyRandomMsg randomGenerator

-- MyRandomMsg vil være en del av din msg-typen som håndterer det nye tallet
type Msg = MyRandomMsg Int

-- Når nummeret er generert, håndter det i din update-funksjon
update : Msg -> Model -> (Model, Cmd Msg)
update (MyRandomMsg number) model =
    ({ model | randomNumber = Just number }, Cmd.none)
```

Eksempelutdata vil være et tilfeldig heltall mellom 1 og 100.

## Deep Dive
Random-nummergenerering i Elm er funksjonell og rein, noe som betyr at det ikke kan gi forskjellige verdier hver gang det kalles. Istedenfor, bruker den en `Seed` som blir oppdatert hver gang en ny verdi genereres. Dette startet med Random-modulen i Elm 0.17 og har utviklet seg for å bli mer uttrykksfullt og kraftfullt i dagens versjoner.

Mens `Random.int` gir et tilfeldig heltall, finnes det flere funksjoner som `Random.float` eller `Random.list` for andre typer av tilfeldighet. Alternativer til Elm sin innebyggede tilfeldighetsgenerator inkluderer å bruke JavaScript gjennom ports eller å implementere dine egne tilfeldighetsalgoritmer.

I detalj, når du bruker `Random.generate`, sender du en kommando til Elm's runtime som håndterer den faktiske tallgenereringen og sender resultatet tilbake til din applikasjon som en melding (`Msg`), noe som lar deg holde applikasjonen din funksjonell og predikerbar.

## See Also
Elm's Random-modul dokumentasjon: https://package.elm-lang.org/packages/elm/random/latest/

En god artikkel om bruk av Random-modulen: https://medium.com/@_rchaves_/random-numbers-in-elm-64ae4d25734

Elm-discourse tråd om tilfeldighetsgenerering: https://discourse.elm-lang.org/t/randomness-in-elm/207
