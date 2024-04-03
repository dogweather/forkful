---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:28.039311-07:00
description: "JSON (JavaScript Object Notation) is een tekstopmaak voor gegevensuitwisseling,\
  \ vergelijkbaar met XML, maar lichter en beter leesbaar. Elm-programmeurs\u2026"
lastmod: '2024-03-13T22:44:50.746421-06:00'
model: gpt-4-0125-preview
summary: JSON (JavaScript Object Notation) is een tekstopmaak voor gegevensuitwisseling,
  vergelijkbaar met XML, maar lichter en beter leesbaar.
title: Werken met JSON
weight: 38
---

## Hoe te:
Elm handelt JSON af met behulp van de `Json.Decode` en `Json.Encode` modules. Hier is een eenvoudig voorbeeld:

```Elm
import Html exposing (text)
import Json.Decode exposing (string)

-- Het decoderen van een eenvoudige JSON-string
jsonString : String
jsonString = "{\"name\": \"Elm\"}"

type alias Gebruiker =
    { naam : String }

gebruikersNaamDecoder : Json.Decode.Decoder String
gebruikersNaamDecoder =
    Json.Decode.field "name" string

main =
    case Json.Decode.decodeString gebruikersNaamDecoder jsonString of
        Ok naam ->
            text ("Welkom, " ++ naam)

        Err _ ->
            text "Oeps, er is iets misgegaan!"
```
Output: 
```
Welkom, Elm
```

## Diepere Duik
Sinds de vroege jaren 2000 is JSON de facto de standaard geworden voor web-API's en heeft het XML verdrongen vanwege de eenvoud. Hoewel Elm beknopt en type-veilig is, kan het omgaan met JSON omslachtig zijn door de noodzaak van expliciete decoders.

Alternatieven zoals Haskell gebruiken typeklassen voor het coderen/decoderen van JSON, wat meer out-of-the-box functionaliteit biedt. Echter, Elm's aanpak helpt typeveiligheid te handhaven en runtime-fouten te voorkomen. Decoders geven expliciet aan hoe JSON naar Elm-typen moet worden geconverteerd, en encoders doen het omgekeerde proces.

## Zie Ook
Voor verdere lectuur en bronnen:

- De officiële JSON-gids van Elm: [Werken met JSON in Elm](https://guide.elm-lang.org/effects/json.html)
- Json.Decode documentatie: [Elm Json.Decode](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode)
- Json.Encode documentatie: [Elm Json.Encode](https://package.elm-lang.org/packages/elm/json/latest/Json-Encode)
