---
date: 2024-01-26 04:21:21.388068-07:00
description: "Hur man g\xF6r: Elm har ingen inbyggd TOML-tolkare, men du kan samverka\
  \ med JavaScript eller anv\xE4nda ett community-paket. S\xE5 h\xE4r kan du tolka\
  \ TOML med hj\xE4lp\u2026"
lastmod: '2024-03-13T22:44:37.852923-06:00'
model: gpt-4-0125-preview
summary: "Elm har ingen inbyggd TOML-tolkare, men du kan samverka med JavaScript eller\
  \ anv\xE4nda ett community-paket."
title: Att arbeta med TOML
weight: 39
---

## Hur man gör:
Elm har ingen inbyggd TOML-tolkare, men du kan samverka med JavaScript eller använda ett community-paket. Så här kan du tolka TOML med hjälp av ett hypotetiskt `elm-toml`-paket:

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

För att dekoda specifika värden:

```elm
portDecoder : Toml.Decode.Decoder Int
portDecoder =
    Toml.Decode.field "server" (Toml.Decode.field "port" Toml.Decode.int)

port : Result String Int
port =
    Toml.decodeString portDecoder configToml
```

Ett exempel på utdata för `port` kan vara `Ok 8080` om dekodningen lyckas.

## Djupdykning
TOML skapades av Tom Preston-Werner, medgrundare av GitHub, som ett enkelt språk för konfigurationsfiler. Det konkurrerar med YAML och JSON; TOML:s syntax syftar till det bästa av båda världarna med fokus på att vara lätt för människor att läsa och skriva.

I Elm, för att hantera TOML, behöver du vanligtvis gå genom JavaScript-samverkan, vilket kan vara lite av ett krångel. Lyckligtvis är Elm-communityn resursfull, och flera paket från tredje part finns. Det hypotetiska `elm-toml`-paketet skulle troligen använda Elms `Port` för att prata med en JavaScript TOML-tolk eller implementera tolkningen direkt i Elm.

Det stora hindret i Elm är att allting är statiskt typat, så du måste skriva anpassade dekodrar för att hantera olika datastrukturer inom TOML, vilket kan vara lite omständligt men tillför säkerhet.

## Se även
För specifikationer och mer info om TOML själv, kolla in [TOML](https://toml.io).
Om du letar efter ett praktiskt tillvägagångssätt till Elm och JavaScript-samverkan, börja med den officiella guiden: [Elm Ports](https://guide.elm-lang.org/interop/ports.html).
För community-paket eller för att bidra, bläddra i [Elm Packages](https://package.elm-lang.org/).
