---
title:                "Elm: Analysering av HTML"
simple_title:         "Analysering av HTML"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/parsing-html.md"
---

{{< edit_this_page >}}

## Hvorfor 

Å parsere HTML, eller konvertere HTML-kode til et datatrennstruktur, er en viktig del av webutvikling. Det gjør det mulig for nettlesere å tolke og vise nettstedene våre på en riktig måte.

## Hvordan du gjør det

For å parse HTML i Elm, må vi bruke et bibliotek kalt `elm/html`. Her er et enkelt eksempel på hvordan vi kan parse en enkel HTML-fil og få ut innholdet i `<body>` taggen:

```elm
import Html exposing (..)
import Html.Parser exposing (parse)
import Html.Attributes exposing (..)

htmlString : String
htmlString =
"""
<html>
  <head>
    <title>Elm HTML parser</title>
  </head>
  <body>
    <h1>Hello World!</h1>
  </body>
</html>
"""

parsedHtml = parse htmlString
bodyContent = parsedHtml
    |> Result.map .node
    |> Result.andThen extractBody
    |> Result.map .children
    |> Result.withDefault []

extractBody : Html.Node -> Result String Html.Node
extractBody html =
    case html of 
        Element "body" attrs children ->
            Ok (Html.text "Found body")
        _ ->
            Err "No body tag found"

main =
    div 
        [] 
        bodyContent
```

Dette vil gi følgende utdata:

```elm
[ h1 [] [ text "Hello World!" ] ]
```

## Dypere dykk

Det er viktig å merke seg at HTML-parsing i Elm er gjort på en strengere måte enn i mange andre språk. Dette er for å sikre at HTML-en som blir parsert er gyldig og ikke vil føre til uventet oppførsel på nettstedet vårt.

Dette betyr også at vi må være ekstra årvåkne når vi skal parse HTML som ikke er skrevet av oss selv eller som kan være ugyldig.

En annen viktig ting å merke seg er at Elm ikke støtter dynamisk HTML-parsing. Dette betyr at vi ikke kan parse HTML mens vi kjører programmet vårt, som for eksempel i en AJAX-forespørsel. Dette går imot Elm sin filosofi om å ha en forutsigbar og feilfri kode.

## Se også

- [Elm HTML bibliotek dokumentasjon](https://package.elm-lang.org/packages/elm/html/latest/)
- [W3Schools HTML tutorial](https://www.w3schools.com/html/)
- [Offisiell Elm nettside](https://elm-lang.org/)