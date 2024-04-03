---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:40.580332-07:00
description: "Afronden van getallen is het aanpassen van een decimaal naar de dichtstbijzijnde\
  \ hele waarde of naar een gespecificeerd aantal fractionele cijfers.\u2026"
lastmod: '2024-03-13T22:44:50.719462-06:00'
model: gpt-4-0125-preview
summary: Afronden van getallen is het aanpassen van een decimaal naar de dichtstbijzijnde
  hele waarde of naar een gespecificeerd aantal fractionele cijfers.
title: Afronden van getallen
weight: 13
---

## Wat & Waarom?

Afronden van getallen is het aanpassen van een decimaal naar de dichtstbijzijnde hele waarde of naar een gespecificeerd aantal fractionele cijfers. Programmeurs ronden af om complexiteit te verminderen, leesbaarheid te verbeteren of om precisievereisten te halen.

## Hoe:

Elm's `Basics` module biedt handige functies voor afronden: `round`, `floor` en `ceiling`. Hier is hoe je ze gebruikt.

```elm
import Basics exposing (round, floor, ceiling)

-- Afronden naar het dichtstbijzijnde gehele getal
round 3.14    --> 3
round 3.5     --> 4

-- Naar beneden afronden
floor 3.999   --> 3

-- Naar boven afronden
ceiling 3.001 --> 4

-- Decimalen afsnijden zonder afronden
truncate 3.76 --> 3
```

Elm biedt ook `toLocaleString` voor het afronden naar een vast aantal decimalen:

```elm
import Float exposing (toLocaleString)

-- Afronden naar twee decimalen
toLocaleString 2 3.14159 --> "3.14"
```

## Diepgaand

Elm is een sterk getypeerde functionele taal die bij-effecten naar de "randen" van de architectuur verplaatst. Dit betekent dat functies als afronden puur en voorspelbaar moeten zijn. Historisch gezien is afronden een veelvoorkomende handeling in veel programmeertalen die te maken hebben met de onnauwkeurigheid van floating-point rekenkunde.

Elm's benadering van afronden is eenvoudig - de functies zijn puur en houden zich aan wiskundige definities voor afronden, naar beneden afronden en naar boven afronden. Elm voorziet in de gemeenschappelijke behoeften door ingebouwde functies te bieden, aangezien precisiebeheer een veelvoorkomende vereiste is, vooral in de financiële wereld en grafische vormgeving.

Alternatieven voor Elm's ingebouwde functies kunnen bestaan uit aangepaste implementaties met behulp van rekenkundige bewerkingen, maar dat voegt onnodige complexiteit toe wanneer de standaardbibliotheek de taak al efficiënt uitvoert.

In de huidige versie gebruikt Elm de onderliggende floating-point wiskunde van JavaScript voor deze bewerkingen, waarmee het consistent blijft met de IEEE 754-norm, wat iets is om te onthouden wanneer je precisie en potentiële floating-point fouten overweegt.

## Zie Ook

- Elm's officiële `Basics` module documentatie: https://package.elm-lang.org/packages/elm/core/latest/Basics
- Een gedetailleerd inzicht in hoe floating-point getallen werken in de computerwetenschap: https://floating-point-gui.de/
- Elm `Float` module voor meer floating-point bewerkingen: https://package.elm-lang.org/packages/elm/core/latest/Float
