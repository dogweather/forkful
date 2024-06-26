---
date: 2024-01-20 17:55:52.061702-07:00
description: "How to: Elm \xE4r designat f\xF6r webbprogram och saknar direkt tillg\xE5\
  ng till kommandoradsargument som server-sidans spr\xE5k har. Men vi kan emulera\
  \ en\u2026"
lastmod: '2024-03-13T22:44:37.844941-06:00'
model: gpt-4-1106-preview
summary: "Elm \xE4r designat f\xF6r webbprogram och saknar direkt tillg\xE5ng till\
  \ kommandoradsargument som server-sidans spr\xE5k har."
title: "L\xE4sa in kommandoradsargument"
weight: 23
---

## How to:
Elm är designat för webbprogram och saknar direkt tillgång till kommandoradsargument som server-sidans språk har. Men vi kan emulera en interaktion med nedanstående exempel.

Först, installera `elm` och skapa ett nytt projekt:

```bash
npm install -g elm
elm init my-project
cd my-project
```

Lägg till en fil `index.html` som laddar ditt Elm-program:

```html
<!DOCTYPE html>
<html>
<head>
  <title>Elm Command Line Arguments</title>
  <script src="elm.js"></script>
</head>
<body>
  <script>
    Elm.Main.init({ flags: "Din initiala data här" });
  </script>
</body>
</html>
```

Och här är `Main.elm`, där vi hanterar "argumentet":

```Elm
module Main exposing (..)

import Browser
import Html exposing (text)

type alias Model = String

init : String -> (Model, Cmd msg)
init flags =
    (flags, Cmd.none)

type Msg = NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update _ model =
    (model, Cmd.none)

view : Model -> Html msg
view model =
    text ("Kommandoradsargumentet var: " ++ model)

main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
```

Kompilera Elm till JavaScript:

```bash
elm make src/Main.elm --output=elm.js
```

Öppna nu `index.html` i webbläsaren. Du ser texten: "Kommandoradsargumentet var: Din initiala data här".

## Deep Dive
Elm är ett funktionellt språk skapat för webbutveckling, särskilt för främjsidesapplikationer, där kommandoradsargument inte är applicerbart direkt som i server-side språk som Python eller Node.js. Tidigare språk som Haskell och F# har inspirerat Elms syntax och funktionell stil, men Elm fokuserar mer på webben än allmän systemprogrammering.

Alternativ för att hantera extern input kan inkludera att ta emot argument via webbadresser (URLs) eller genom en webbtjänst där Elmapplikationen kan hämta data vid körning.

För en regelrätt interaktion med systemkommandon och för att läsa kommandoradsargument direkt, överväg att använda Node.js med Elm, eller ett annat backend-språk som kan kommunicera med din Elm-klient.

## See Also
- Elm Official Guide: https://guide.elm-lang.org/
- Elm Packages: https://package.elm-lang.org/
- Elm och Node.js Integration: https://elm-lang.org/news/porting-to-elm-0.19#nodejs-integration
