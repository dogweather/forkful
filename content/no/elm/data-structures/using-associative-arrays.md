---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:52.532928-07:00
description: "Hvordan: I Elm jobber du med Ordb\xF8ker i `Dict`-modulen, s\xE5 la\
  \ oss dykke inn i et raskt eksempel."
lastmod: '2024-03-13T22:44:40.701435-06:00'
model: gpt-4-0125-preview
summary: "I Elm jobber du med Ordb\xF8ker i `Dict`-modulen, s\xE5 la oss dykke inn\
  \ i et raskt eksempel."
title: Bruke associative tabeller
weight: 15
---

## Hvordan:
I Elm jobber du med Ordbøker i `Dict`-modulen, så la oss dykke inn i et raskt eksempel:

```Elm
import Dict exposing (Dict)

-- Initialiserer en ordbok med String-nøkler og Int-verdier
exampleDict : Dict String Int
exampleDict = Dict.fromList [("apple", 5), ("banana", 2), ("orange", 8)]

-- Legger til eller oppdaterer en verdi
updatedDict = Dict.insert "grape" 10 exampleDict

-- Henter en verdi (legg merke til Maybe-typen, ettersom nøkkelen kanskje ikke er til stede)
fruitCount : Maybe Int
fruitCount = Dict.get "apple" updatedDict

-- Fjerner et nøkkel-verdi-par
finalDict = Dict.remove "banana" updatedDict

-- Konverterer en ordbok tilbake til en liste
dictToList = Dict.toList finalDict
```

Eksempelutdata når du viser `dictToList`:

```Elm
[("apple", 5), ("grape", 10), ("orange", 8)]
```

Dette demonstrerer de grunnleggende operasjonene: å opprette, oppdatere, få tilgang til og iterere over en Ordbok.

## Dypdykk
Ordbøker i Elm bruker internt en struktur kjent som et AVL-tre - en type selvbalanserende binært søketre. Dette valget skaper en balanse mellom å sikre at operasjoner som insert, get og remove har god ytelse (logaritmisk tidskompleksitet) og vedlikeholder enkelhet i håndteringen av dataene.

Til tross for styrkene til Elms `Dict`, er det ikke en løsning som passer for alle. For samlinger som er ordnet eller trenger å itereres over sekvensielt, kan Liste eller Array være mer passende. Videre, når man jobber med et fast sett av kjente nøkler, kan bruk av egendefinerte typer (Elms versjon av enums) tilby mer typesikkerhet og klarere intensjon i koden din.

I Elm-økosystemet tilbyr `Dict` en pålitelig måte å håndtere samlinger av nøkkel-verdi-par hvor nøklene er unike og rekkefølgen ikke betyr noe. Mens nyere eller mer avanserte strukturer kan dukke opp, forblir `Dict`-modulen et grunnleggende verktøy i Elm-programmererens verktøykasse for sin enkelhet og effektivitet i håndteringen av assosiative tabeller.
