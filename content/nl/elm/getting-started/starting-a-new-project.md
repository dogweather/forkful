---
title:                "Een nieuw project starten"
aliases: - /nl/elm/starting-a-new-project.md
date:                  2024-01-28T22:08:14.065613-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een nieuw project starten"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/elm/starting-a-new-project.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een nieuw project starten in Elm betekent het opzetten van een schone lei voor het bouwen van betrouwbare webapps. Programmeurs doen dit om te profiteren van de eenvoud en robuustheid van Elm, vooral voor projecten die geen runtime-uitzonderingen vereisen.

## Hoe te:

In Elm, begin je met het `elm init` commando. Navigeer naar je projectmap en start je terminal:

```shell
mkdir mijn-elm-project
cd mijn-elm-project
elm init
```

Dit commando maakt een `elm.json` bestand en `src` map aan. Hier is een simpel "Hallo, Wereld!" in Elm:

```Elm
module Main exposing (..)

import Html exposing (text)

main =
    text "Hallo, Wereld!"
```

Wanneer je dit uitvoert met `elm reactor` en je bezoekt `http://localhost:8000`, wordt "Hallo, Wereld!" in je browser getoond.

## Diepere Duik

Elm kwam rond in 2012, met als doel front-end ontwikkeling aangenamer te maken. Het gaat niet alleen om het vermijden van runtimefouten; Elm brengt een sterke focus op eenvoud en ontwikkelaarsgeluk. In tegenstelling tot veel alternatieven, zoals het schrijven van pure JavaScript of het gebruiken van raamwerken zoals React, is Elm een eigen taal. Met sterke typen en pure functies, brengt het voorspelbaarheid en onderhoudbaarheid op tafel.

Wanneer je een nieuw Elm-project start, omarm je ook de Elm-architectuur, een patroon voor het structureren van je webapps dat eenvoud en schaalbaarheid benadrukt. Het bundelt je volledige applicatiestaat en hoe deze bijwerkt. Andere hulpmiddelen zoals `create-elm-app` kunnen ingewikkeldere opstellingen maken, maar beginnen met `elm init` is zo slank als het maar kan.

## Zie Ook

- Elm Officiële Gids: https://guide.elm-lang.org/
- Elm Architectuur Tutorial: https://guide.elm-lang.org/architecture/
- Elm Gereedschap: `create-elm-app`: https://github.com/halfzebra/create-elm-app
- Elm Pakketcatalogus: https://package.elm-lang.org/
