---
date: 2024-01-26 04:21:21.388068-07:00
description: "TOML, som st\xE5r f\xF6r Toms Uppenbara, Minimala Spr\xE5k, \xE4r ett\
  \ data-serialiseringsspr\xE5k. Elm-programmerare anv\xE4nder det f\xF6r att hantera\
  \ konfigurationsdata\u2026"
lastmod: 2024-02-19 22:04:57.065269
model: gpt-4-0125-preview
summary: "TOML, som st\xE5r f\xF6r Toms Uppenbara, Minimala Spr\xE5k, \xE4r ett data-serialiseringsspr\xE5\
  k. Elm-programmerare anv\xE4nder det f\xF6r att hantera konfigurationsdata\u2026"
title: Att arbeta med TOML
---

{{< edit_this_page >}}

## Vad & Varför?
TOML, som står för Toms Uppenbara, Minimala Språk, är ett data-serialiseringsspråk. Elm-programmerare använder det för att hantera konfigurationsdata eftersom det är läsligt för människor och kartlägger snyggt till nyckel-värdepar som behövs i applikationer.

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
