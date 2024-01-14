---
title:                "Elm: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Varför

Att läsa in kommandoradsargument är en viktig del av programmering, särskilt i funktionella programmeringsspråk som Elm. Det hjälper dig att skapa mer flexibla och anpassningsbara program som kan hantera olika inmatningar.

# Så här gör du

För att läsa in kommandoradsargument i Elm behöver du använda en modul som heter `Platform.Cmd` och dess `args` funktion. Här är ett exempel på hur du kan göra det:

```
elm package install elm/core
```

```elm
module Main exposing (..)

import Platform.Cmd exposing (args)

main =
    Cmd.map handleArgs args

handleArgs : List String -> Maybe String
handleArgs args =
    case List.head args of
        Just arg ->
            Just ("Det första argumentet är: " ++ arg)

        Nothing ->
            Nothing
```

I det här exemplet använder vi `Cmd.map` för att applicera vår `handleArgs` funktion på de argument som vi får från `args`. Funktionen `handleArgs` tar emot en lista med strängar och returnerar det första argumentet som en `Maybe String`. Om inga argument finns returneras `Nothing`.

Om du kompilerar och kör programmet med ett argument, till exempel `elm-make Main.elm --output main.js`, så kommer du att få följande output: `Det första argumentet är: Main.elm`.

# Djupdykning

Det finns flera olika sätt att läsa in och hantera kommandoradsargument i Elm, beroende på dina specifika behov. En alternativ metod är att använda paketet `elm-community/argv`, som erbjuder mer avancerade funktioner för att hantera argument.

Det är också viktigt att tänka på att kommandoradsargument endast fungerar för program som körs i en terminal eller kommandoradsgränssnitt, och kan inte användas för webbapplikationer.

# Se också
- [Elm Dokumentation: Platform.Cmd.args](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd#args)
- [Elm-paketet "elm-community/argv"](https://package.elm-lang.org/packages/elm-community/argv/latest/)