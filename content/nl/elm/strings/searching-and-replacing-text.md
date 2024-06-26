---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:05.374130-07:00
description: 'Hoe: In Elm kun je de `String`-module gebruiken om delen van een string
  te vervangen. Laten we het in actie zien.'
lastmod: '2024-03-13T22:44:50.709641-06:00'
model: gpt-4-0125-preview
summary: In Elm kun je de `String`-module gebruiken om delen van een string te vervangen.
title: Tekst zoeken en vervangen
weight: 10
---

## Hoe:
In Elm kun je de `String`-module gebruiken om delen van een string te vervangen. Laten we het in actie zien:

```Elm
import String

replaceVoorbeeld : String
replaceVoorbeeld =
    String.replace "kat" "hond" "De kat zat op de mat"

-- De uitvoer zal zijn: "De hond zat op de mat"
```

## Diepgaande Verkenning
Elm's manier om zoek-en-vervang voor strings te hanteren is vrij eenvoudig, vergelijkbaar met andere functionele talen. Het gebruikt standaard geen reguliere expressies voor dit doel in de kern taal, in tegenstelling tot talen zoals JavaScript. Deze eenvoud is bewust gekozen om Elm's doelen van betrouwbaarheid en onderhoudbaarheid te behouden.

Historisch gezien streeft Elm ernaar om een robuuste set ingebouwde functies te bieden die veelvoorkomende taken aanpakken, en zoek-vervang is niet anders. Elm's `String`-module is er al sinds de vroege dagen, hoewel deze veranderingen heeft ondergaan naarmate de taal zich ontwikkelde.

Alternatieven voor het gebruik van de `String.replace`-functie kunnen zijn: het schrijven van je eigen zoek-en-vervang-logica of het toevoegen van een extra pakket dat Elm's mogelijkheden voor stringmanipulatie uitbreidt, zoals op regex gebaseerd zoeken.

Wat betreft de implementatie is Elm's `String.replace`-functie puur. Dat betekent dat het altijd dezelfde uitvoer produceert voor een gegeven invoer en geen bijeffecten heeft - een kernprincipe in Elm's ontwerp. Het gebruikt een efficiënt algoritme onder de motorkap, maar de taal abstraheert de complexiteit weg zodat je je kunt concentreren op coderen zonder je druk te maken over kleine dingen.

## Zie Ook
- Elm `String`-module documentatie: https://package.elm-lang.org/packages/elm/core/latest/String
- Een introductie tot regex in Elm met het elm/regex pakket: https://package.elm-lang.org/packages/elm/regex/latest
- Stringverwerking in functioneel programmeren: https://en.wikipedia.org/wiki/Functional_programming
