---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:52.566746-07:00
description: "Hoe: Elm heeft geen ingebouwde TOML-parser, maar je kunt interopereren\
  \ met JavaScript of een community-pakket gebruiken. Hier is hoe je TOML zou kunnen\u2026"
lastmod: '2024-03-13T22:44:50.748588-06:00'
model: gpt-4-0125-preview
summary: Elm heeft geen ingebouwde TOML-parser, maar je kunt interopereren met JavaScript
  of een community-pakket gebruiken.
title: Werken met TOML
weight: 39
---

## Hoe:
Elm heeft geen ingebouwde TOML-parser, maar je kunt interopereren met JavaScript of een community-pakket gebruiken. Hier is hoe je TOML zou kunnen parsen met een hypothetisch `elm-toml` pakket:

```elm
import Toml

configToml : String
configToml =
    """
    [server]
    port = 8080
    """

parseResult : Result Toml.Decode.Error Toml.Value
parseResult =
    Toml.decodeString configToml
```

Voor het decoderen van specifieke waarden:

```elm
portDecoder : Toml.Decode.Decoder Int
portDecoder =
    Toml.Decode.field "server" (Toml.Decode.field "port" Toml.Decode.int)

port : Result String Int
port =
    Toml.decodeString portDecoder configToml
```

Voorbeelduitvoer voor `port` zou `Ok 8080` kunnen zijn als het decoderen slaagt.

## Diepere duik
TOML is gecreëerd door Tom Preston-Werner, mede-oprichter van GitHub, als een eenvoudige taal voor configuratiebestanden. Het concurreert met YAML en JSON; TOML's syntaxis streeft naar het beste van beide werelden met een focus op gemakkelijk lees- en schrijfbaar zijn voor mensen.

In Elm, om TOML te kunnen hanteren, moet je meestal door JavaScript-interoperabiliteit heen, wat een beetje een gedoe kan zijn. Gelukkig is de Elm-gemeenschap vindingrijk en bestaan er meerdere pakketten van derden. Het hypothetische `elm-toml` pakket zou waarschijnlijk Elm's `Port` gebruiken om met een JavaScript TOML-parser te praten of de parsing direct in Elm implementeren.

De voornaamste hinderpaal in Elm is dat alles statisch getypeerd is, dus je zult aangepaste decoders moeten schrijven om verschillende datastructuren binnen TOML te kunnen hanteren, wat een beetje omslachtig kan zijn maar wel veiligheid toevoegt.

## Zie Ook
Voor specificaties en meer informatie over TOML zelf, bekijk [TOML](https://toml.io).
Als je op zoek bent naar een praktische benadering van Elm en JavaScript-interoperabiliteit, begin dan met de officiële gids: [Elm Ports](https://guide.elm-lang.org/interop/ports.html).
Voor communitypakketten of om bij te dragen, blader door [Elm Packages](https://package.elm-lang.org/).
