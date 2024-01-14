---
title:                "Elm: Läsning av kommandoradsargument"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Varför läsa kommandoradsargument i Elm?

När man skriver program i Elm är det ofta viktigt att kunna läsa in data från användaren. Ett sätt att göra det är genom att läsa kommandoradsargument. Detta gör det möjligt att anpassa programmet och ge det olika beteenden baserat på vad användaren anger vid start. I denna blogginlägg ska vi titta på hur man kan läsa kommandoradsargument i Elm och även ge en djupare förståelse för konceptet.

## Så här gör du

För att läsa kommandoradsargument i Elm behöver du importera paketet "elm-community/html-extra" som tillhandahåller ett modul för att hantera kommandoradsargument. Sedan kan du använda funktionen "Html.Attributes.get" för att hämta de argument som skickas med när programmet körs.

```Elm
import Html.Extra exposing (CommandLineArgs)
import Html.Attributes

main : Program CommandLineArgs
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
```

För att läsa ett specifikt argument kan vi använda funktionen "Html.Attributes.get" tillsammans med namnet på argumentet som en sträng. Om vi till exempel vill läsa argumentet "mode" som anger vilket läge programmet ska köras i, kan vi göra så här:

```Elm
init : CommandLineArgs -> ( Model, Cmd Msg )
init args =
    let
        mode =
            Html.Attributes.get "mode" args |> Maybe.withDefault "default"
    in
    ( { model | mode = mode }, Cmd.none )
```

När programmet körs kan vi nu skicka med argument "mode" som t.ex. "prod" eller "dev" för att bestämma vilket beteende programmet ska ha.

## Djupdykning

Kommandoradsargument kan vara mycket användbara när man utvecklar program i Elm. De ger möjlighet att anpassa programmet utan att behöva ändra koden. Det kan vara användbart i tester, debugging eller för att ge programmet olika beteenden beroende på var det körs.

Det är också möjligt att läsa flera argument samtidigt genom att använda funktionen "Html.Attributes.getAll". Denna funktion returnerar alla argument som skickas med och spara dem i en lista.

En sak att tänka på är att kommandoradsargument endast fungerar när programmet körs i en webbläsare. Om du vill läsa argument när programmet körs utanför en webbläsare, måste du hitta en annan lösning, t.ex. att läsa från en textfil.

## Se även

- Elm Community HTML Extra - https://package.elm-lang.org/packages/elm-community/html-extra/latest/
- Elm Documentation: CommandLineArgs - https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd#CommandLineArgs
- Codecademy: Command Line Arguments in Elm - https://www.codecademy.com/articles/command-line-arguments-in-elm