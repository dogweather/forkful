---
date: 2024-01-20 15:14:10.019563-07:00
description: "Hur man g\xF6r: ."
lastmod: '2024-03-13T22:44:37.839943-06:00'
model: unknown
summary: .
title: "Att h\xE4mta aktuellt datum"
weight: 29
---

## Hur man gör:
```Elm
import Time
import Task
import Browser

type Msg = GotTime Time.Posix

getTime : Cmd Msg
getTime =
    Task.perform GotTime Time.now

main =
    Browser.element
        { init = \_ -> ( {}, getTime )
        , view = \_ -> Html.text ""
        , update = \msg -> \model -> (model, Cmd.none)
        , subscriptions = \_ -> Sub.none
        }

```
Körningsexempel (output varierar):
```
{ timestamp = 1615125600000 }
```

## Fördjupning
I Elm hanteras tiden genom `Time`-modulen och den bygger på POSIX-tidsstandard. Tidigare kunde man få nuvarande datum genom att använda JavaScript, men nu görs det mer Elm-idiomatiskt genom att anropa `Time.now`. Alternativen inkluderar att använda externa paket eller att skicka in tiden från JavaScript via ports, men `Time.now` är standard och enkel. Vidare hanterar Elm tidszoner genom att omvandla all tid till UTC.

## Se även
- Elm Time documentation: https://package.elm-lang.org/packages/elm/time/latest/
- An article about Elm and time management: https://medium.com/elm-shorts/working-with-time-in-elm-8ec8fa854dd5
