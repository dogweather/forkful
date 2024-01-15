---
title:                "Läsning av kommandoradsargument"
html_title:           "Elm: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Att läsa argument från kommandoraden kan vara användbart när du behöver lägga till interaktionsmöjligheter till ditt program, till exempel när du behöver ta emot användarinmatning innan du kör koden.

## Hur man gör

För att läsa argument från kommandoraden i Elm, använder man "Commands"-modulen. Först importera den i din kod:

```Elm
import Commands exposing (..)
```

Sedan kan du definiera en funktion som tar emot en lista av argument och utför önskade operationer, till exempel att skriva ut dem:

```Elm
printArgs : List String -> Cmd msg
printArgs args =
    case args of
        [] ->
            Cmd.succeed ()

        arg :: rest ->
            Cmd.batch
                [ Cmd.perform (log arg)
                , printArgs rest
                ]
```

När vi använder funktionen "Cmd.perform" behöver vi ge den ett felmeddelande som generisk typ, eftersom vi inte förväntar oss att få någon specifik tillbakasändning.

För att faktiskt köra funktionen, använd "Commands"-modulens "run" funktion tillsammans med "Platform"-paketet:

```Elm
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "", Commands.run printArgs )
```

Nu när vi kör programmet, kan vi ge det argument från kommandoradens terminal:

```
elm reactor 1, 2, 3
```

Ovanstående kommer att skriva ut "1", "2" och "3" en efter en på konsolen.

## Fördjupning

I det här exempel använde vi bara en CMD för att skriva ut argumenten. Men möjligheterna är oändliga, du kan till exempel ta emot specifika argument och utföra olika operationer beroende på vilka argument som ges.

Det är också värt att notera att Elm inte tillåter att läsa från stdin, så det är inte möjligt att få användarinmatning med hjälp av detta sätt. Istället får vi använda oss av JavaScript för att få tillgång till konsolen.

## Se också

- Elm "Commands" modul dokumentation: [https://package.elm-lang.org/packages/elm/core/latest/Commands](https://package.elm-lang.org/packages/elm/core/latest/Commands)
- "Platform" paketet dokumentation: [https://package.elm-lang.org/packages/elm/browser/latest/](https://package.elm-lang.org/packages/elm/browser/latest/)
- Elm programmeringsspråkets officiella hemsida: [https://elm-lang.org/](https://elm-lang.org/)