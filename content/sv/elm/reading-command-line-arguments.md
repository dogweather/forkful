---
title:    "Elm: Läsning av kommandoradsargument"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Varför

Att läsa kommandoradsargument är en viktig del av programmering då det ger möjlighet att interagera med en applikation på ett flexibelt sätt. Läs vidare för att lära dig hur du kan läsa och använda kommandoradsargument i dina Elm-program.

## Så här gör du

För att läsa kommandoradsargument i Elm, använd funktionen `Platform.worker` tillsammans med `Platform.SendToApp` modulen. Se nedan för ett kodexempel:

```Elm
import Platform
import Platform.SendToApp

type Msg = ArgumentsReceived (List String)

main : Program () Model Msg
main =
  Platform.worker
    { init = init
    , update = update
    , subscriptions = subscriptions
    }

init : () -> ( Model, Cmd Msg )
init _ =
  ( Model, Platform.SendToApp.NoOp )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ArgumentsReceived arguments ->
      ( { model | arguments = arguments }, Platform.SendToApp.NoOp )

subscriptions : Model -> Sub Msg
subscriptions _ =
  Platform.Sub.batch [ Platform.Sub.map ArgumentsReceived Platform.Sub.args ]
```

I exemplet ovan används `Platform.Sub.args` för att läsa in argumenten och sedan skickas de tillbaka till applikationen via `Platform.SendToApp` modulen.

## Djupdyk

När du läser kommandoradsargument i Elm måste du vara medveten om skillnader mellan olika operativsystem. Till exempel skiljer sig syntaxen för att öppna en fil mellan Windows och Unix-system. Det är också viktigt att hantera eventuella felaktiga eller saknade argument för att undvika att applikationen kraschar.

## Se också

För mer information om kommandoradsargument i Elm, kolla in följande länkar:

- [Elm dokumentation](https://elm-lang.org/docs)
- [Läsning av filer i Elm](https://guide.elm-lang.org/interop/file_system.html)
- [Elm paket för hantering av kommandoradsargument](https://package.elm-lang.org/packages/elm-command-line/1.1.0/)