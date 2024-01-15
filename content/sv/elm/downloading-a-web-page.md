---
title:                "Ladda ner en webbsida"
html_title:           "Elm: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Varför

Varför skulle man ladda ner en webbsida? Förmodligen för att få tillgång till den offline eller för att utföra någon typ av analys på dess innehåll.

## Så här gör du

```Elm
import Http

-- Skapa en funktion som tar en URL som argument och returnerar en sträng med webbsidans innehåll
getPageContent : String -> String
getPageContent url =
    let
        -- Skicka en GET-förfrågan till URL:en och få tillbaka en Http.Response
        response = Http.get url

        -- Plocka ut webbsidans innehåll från svaret
        content = case response of
            Http.BadUrl _ ->
                "Ogiltig URL"
            Http.Timeout ->
                "Det tog för lång tid att hämta webbsidan"
            Http.NetworkError ->
                "Ett nätverksfel inträffade"
            Http.BadStatus _ ->
                "Fel statuskod från servern"
            Http.GoodStatus _ ->
                -- Konvertera svaret till JSON, vi antar att webbsidan är skriven i JSON-format
                let
                    json = response
                        |> Http.toBody
                        |> Json.fromString
                        |> Result.withDefault []
                in
                    -- Omvandla JSON till en sträng och returnera den
                    Json.Encode.encode 0 json
    in
        content
```

För att faktiskt ladda ner en webbsida och få tillgång till dess innehåll kan du sedan anropa funktionen `getPageContent` med en giltig URL som argument. Till exempel:

```Elm
getPageContent "https://www.example.com"
```

Detta kommer att returnera en sträng med innehållet på webbsidan, vilket kan användas för att göra analys eller bearbetning av datat.

## Djupdykning

Att ladda ner en webbsida i Elm kan göras med hjälp av paketet `Http`. Genom att använda `Http.get` funktionen kan en GET-förfrågan skickas till en URL och ett Http.Response objekt returneras.

Detta svar innehåller information som statuskod, headers och svarets kropp. Genom att använda `Http.toBody` funktionen kan svaret konverteras till en sträng. Om webbsidan är skriven i JSON-format kan svaret även konverteras till JSON med hjälp av `Json.fromString` funktionen.

Det är viktigt att hantera olika fel som kan uppstå när man försöker ladda ner en webbsida, som ogiltiga URL:er, tidsutlöp och nätverksfel. Genom att använda `case` uttryck kan man hantera dessa olika typer av fel och ge lämpliga meddelanden till användaren.

## Se även

- [Elm Paketdokumentation för Http](https://package.elm-lang.org/packages/elm/http/latest/)
- [Elm Guiden till HTTP-anrop](https://guide.elm-lang.org/effects/http.html)
- [Elm JSON dokumentation](https://package.elm-lang.org/packages/elm/json/latest/)