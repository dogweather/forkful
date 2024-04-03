---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:35.206884-07:00
description: "Debugoutput afdrukken in Elm gaat over het weergeven van waarden in\
  \ de console om te begrijpen wat er in je code gebeurt. We doen dit om bugs op te\
  \ sporen\u2026"
lastmod: '2024-03-13T22:44:50.727488-06:00'
model: gpt-4-0125-preview
summary: Debugoutput afdrukken in Elm gaat over het weergeven van waarden in de console
  om te begrijpen wat er in je code gebeurt.
title: Debug-output afdrukken
weight: 33
---

## Wat & Waarom?

Debugoutput afdrukken in Elm gaat over het weergeven van waarden in de console om te begrijpen wat er in je code gebeurt. We doen dit om bugs op te sporen en ervoor te zorgen dat de logica loopt zoals bedoeld.

## Hoe te:

Elm heeft geen ingebouwde `print` functie zoals sommige talen, maar je kunt de `Debug` module gebruiken voor de console output:

```Elm
import Debug

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  model
    |> Debug.log "model voor update"
    |> actualUpdateFunction msg
    |> Debug.log "model na update"
```

Je zult zoiets zien in de console van je browser:

```
model voor update: { ... enige modelgegevens ... }
model na update: { ... enige bijgewerkte modelgegevens ... }
```

Onthoud, de `Debug.log` functie is handig, maar verzend je code er niet mee. Elm zal je eraan herinneren om debugstatements te verwijderen voordat je een productiebuild kunt maken.

## Uitdieping

`Debug.log` maakt deel uit van de Elm `Debug` module, ontworpen voor alleen assistentie tijdens de ontwikkeling. Historisch gezien heeft Elm de nadruk gelegd op onderhoudbaarheid en foutafhandeling, waarbij de `Debug` module bewust eenvoudig is gelaten. De eenvoud ervan zorgt ervoor dat ontwikkelaars zich richten op betekenisvolle output in plaats van verdwaald te raken in een uitgebreide debugging suite.

De `Debug.log` functie van Elm neemt twee argumenten: een stringtag en de te loggen gegevens. De output wordt vervolgens afgedrukt naar de browserconsole. De alternatieven voor deze aanpak zouden zijn:

1. Traditionele console logging: Elm ondersteunt geen directe console logging vanwege Elm's architectuur die streeft naar nul runtime-uitzonderingen, en directe logging zou deze garantie kunnen verbreken.
2. Elm's Tijdreizende Debugger: Dit hulpmiddel laat je de staat van je applicatie in de loop van de tijd visualiseren zonder console logs en is een krachtige manier om complexe apps te debuggen.

Wat betreft de implementatie, de `Debug.log` functie verpakt je gegevens met een identificatietag. Dit is handig om verschillende gegevenspunten te onderscheiden. In productie zal de Elm-compiler elk gebruik van `Debug.log` markeren, waardoor je je productiecode schoon houdt van debugging artefacten.

## Zie ook

- Elm's officiële gids over debugging: https://guide.elm-lang.org/debugging/
- Introductie van de Tijdreizende Debugger: https://elm-lang.org/news/the-perfect-bug-report
- Elm Debug module documentatie: https://package.elm-lang.org/packages/elm/core/latest/Debug
