---
title:                "Elm: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Varför

Webbsidor är en viktig del av vår vardag och en av de vanligaste sätten att få information och utföra olika uppgifter. Att kunna ladda ner en webbsida är en nyttig färdighet för utvecklare och kan vara användbart för testning och analysering av webbsidor.

## Hur man gör

För att ladda ner en webbsida i Elm, använd funktionen `Http.get` och ange URL:en för den sida du vill hämta.

```Elm
import Http
import String

Http.get "https://www.example.com" -- byt ut med önskad URL
    |> Http.send ReceiveResponse

type Msg = ReceiveResponse (Result Http.Error String)

-- funktion för att hantera hämtat data
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ReceiveResponse (Ok data) ->
            ( { model | pageContent = data }, Cmd.none )

        ReceiveResponse (Err err) ->
            ( { model | pageContent = "Ett fel har inträffat" }, Cmd.none )

-- hämta data vid start
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

-- visa laddad data
view : Model -> Html Msg
view model =
    div [] [ text model.pageContent ]

```

I koden ovan används `Http.send` för att skicka en anmodan om hämtning och `UpdateResponse`-typen för att hantera det hämtade innehållet. I den här enkla implementationen visas bara det laddade innehållet på sidan, men du kan anpassa koden efter dina behov.

## Djupdykning

Om du vill lära dig mer om laddning av webbsidor i Elm, kan du gräva djupare i dokumentationen för `Http`-paketet och utforska andra åtgärder såsom att skicka data och hantera cookies.

## Se även

Läs mer om `Http.get`-funktionen i Elm dokumentationen: https://package.elm-lang.org/packages/elm-lang/http/2.0.0/Http#get

Utforska andra möjligheter för webbutveckling i Elm med `elm-explorer`-guiden: https://guide.elm-lang.org/webapps/