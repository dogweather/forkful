---
date: 2024-01-20 17:55:50.971506-07:00
description: "Czytanie argument\xF3w z linii polece\u0144 to spos\xF3b na przekazywanie\
  \ danych do programu podczas jego uruchamiania. Programi\u015Bci robi\u0105 to,\
  \ by zwi\u0119kszy\u0107\u2026"
lastmod: '2024-03-13T22:44:35.337194-06:00'
model: gpt-4-1106-preview
summary: "Czytanie argument\xF3w z linii polece\u0144 to spos\xF3b na przekazywanie\
  \ danych do programu podczas jego uruchamiania."
title: "Odczytywanie argument\xF3w linii polece\u0144"
weight: 23
---

## How to:
Elm nie obsługuje bezpośrednio czytania argumentów linii poleceń, ponieważ jest językiem skoncentrowanym na aplikacjach webowych. Ale można to osiągnąć za pośrednictwem JavaScript i portów (ports).

```Elm
port module Main exposing (..)

import Browser
import Json.Decode as Decode

port args : (String -> msg) -> Sub msg

type Msg = CmdArgs String

main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> args CmdArgs
        }

init : () -> (Model, Cmd Msg)
init _ =
    (Model "", Cmd.none)

type alias Model =
    { cmdArgs : String }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        CmdArgs argsString ->
            ({ model | cmdArgs = argsString }, Cmd.none)

view : Model -> Html.Html Msg
view model =
    Html.text ("Command line arguments were: " ++ model.cmdArgs)
```

W JavaScript:
```javascript
const flags = process.argv.slice(2).join(' ');
const app = Elm.Main.init({ flags });

app.ports.args.subscribe((args) => {
  console.log('Received args:', args);
});
```

## Deep Dive
W Elm, porty są mostem do JavaScript, pozwalają na komunikację między językami. Przyjmijmy, że Elm jeszcze bardziej naciska na prace z przeglądarką, argumenty linii poleceń są bardziej naturalne dla środowiska Node.js czy inneś języka serwerowego. Chociaż Elm nie jest przeznaczony do skryptów, można wykorzystać porty do tej funkcjonalności.

Alternatywą dla portów może być WebAssembly lub inne narzędzia kompilacji Elm do JS, które umiejętnie obsługują CLI.

Co do historii, Elm powstał głównie dla aplikacji webowych, więc czytanie argumentów z linii poleceń nie było nigdy priorytetem.

## See Also
- Oficjalna strona Elm: [https://elm-lang.org/](https://elm-lang.org/)
- Dokumentacja portów Elm: [https://guide.elm-lang.org/interop/ports.html](https://guide.elm-lang.org/interop/ports.html)
- Node.js dokumentacja `process.argv`: [https://nodejs.org/docs/latest/api/process.html#process_process_argv](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
