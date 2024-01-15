---
title:                "Arbeta med json"
html_title:           "Elm: Arbeta med json"
simple_title:         "Arbeta med json"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/working-with-json.md"
---

{{< edit_this_page >}}

## Varför

JSON (JavaScript Object Notation) är en vanligt använd format för att hantera data. Med hjälp av JSON kan vi enkelt överföra och lagra data i en strukturerad form. Att lära sig att arbeta med JSON i Elm kan göra det enklare för oss att integrera med andra system och hantera stora mängder av data.

## Hur man gör det

För att arbeta med JSON i Elm behöver vi först importera biblioteket "Json.Decode".

```Elm
import Json.Decode exposing (..)
```

För att sedan hämta en JSON-fil från en extern resurs använder vi funktionen "Http.get", som tar två argument: en URL och en avkodare för JSON. Avkodaren följer en struktur som motsvarar JSON-filen.

```Elm
type alias Person = {
    name: String,
    age: Int
}

personDecoder : Decoder Person
personDecoder =
    map2 Person
        (field "name" string)
        (field "age" int)

httpGetExample : Cmd Msg
httpGetExample =
    Http.get "https://example.com/api/person/1" personDecoder
```

När vi sedan tar emot svar från vår anrop kommer det att vara formaterat enligt vår avkodare och vi kan enkelt arbeta med datan. Om vi till exempel vill skriva ut personens namn och ålder kan vi göra det på följande sätt:

```Elm
case result of
    Ok person ->
        Debug.log person.name ++ ", " ++ String.fromInt person.age

    Err error ->
        Debug.log "An error occurred: " ++ toString error
```

## Djupdykning

För att kunna hantera olika typer av data i JSON kan vi använda använda funktioner som "field" eller "at" för att komma åt specifika delar av vår struktur. Om vi till exempel har en JSON-fil med flera personer kan vi hämta ut en lista av dessa genom att använda "at" och en lista av avkodare.

```Elm
personsDecoder : Decoder (List Person)
personsDecoder =
    list personDecoder

httpGetExample : Cmd Msg
httpGetExample =
    Http.get "https://example.com/api/persons" personsDecoder
```

Vi kan även använda funktionen "Json.Decode.andThen" för att hantera felaktiga värden i vår JSON-fil. Genom att använda "Json.Decode.optional" kan vi välja att ignorera en viss del av datan om den saknas eller är ogiltig.

## Se även

- Elm-dokumentation: https://guide.elm-lang.org/effects/http.html
- Json.Decode-dokumentation: https://package.elm-lang.org/packages/elm/json/latest/Json-Decode