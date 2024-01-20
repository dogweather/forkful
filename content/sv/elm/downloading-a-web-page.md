---
title:                "Ladda ner en webbsida"
html_title:           "Bash: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & varför?

Som programmerare kan du behöva ladda ner en webbsida för att bearbeta eller analysera dess innehåll. Detta kan vara användbart för att skrapa data, testa webbdesigner eller kontrollera webbsidans funktionalitet.

## Hur man gör:

Här är ett exempel på hur du kan ladda ner en webbsida i Elm:

```Elm
import Http
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

main =
  Html.beginnerProgram { model = "", view = view, update = update }

type Msg = MorePlease | NewJoke String

update msg model =
  case msg of
    MorePlease ->
      ( model
      , Http.send NewJoke (Http.getString "http://api.icndb.com/jokes/random")
      )

    NewJoke newJoke ->
      ( newJoke, Cmd.none )

view model =
  div []
    [ div [] [ text model ]
    , button [ onClick MorePlease ] [ text "More Please!" ]
    ]
```

När du trycker på knappen "More Please!" kommer en ny skämt att laddas från API:t och visas på webbsidan.

## Fördjupning

Att ladda ner webbsidor i programmering har en lång historia, från tidiga webbskrapare till moderna APIs. Alternativ till Elm för att hämta webbsidor inkluderar andra webbbaserade programmeringsspråk som JavaScript och Python, men Elm utmärker sig för dess robusthet och tydlighet.

Utförandet av webbsidan i Elm är enkel och direkt. Elm använder Http paketet för att skicka en GET begäran till webbsidan och tar sedan emot svaret som en sträng.

## Se även

Om du vill fördjupa dina kunskaper om Elm-programmering kan du kolla in följande källor:

1. [Elm's offical guide](https://guide.elm-lang.org/)
2. [Elm's Github repository](https://github.com/elm)
3. [Elm's package documentation](https://package.elm-lang.org/packages/elm/http/latest/Http)