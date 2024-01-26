---
title:                "JSON-tiedostojen käsittely"
html_title:           "Arduino: JSON-tiedostojen käsittely"
simple_title:         "JSON-tiedostojen käsittely"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON (JavaScript Object Notation) on kevyt dataformaatti. Sitä käytetään datan siirtämiseen palvelimien ja asiakassovellusten välillä. Se on yleinen web-sovelluksissa, koska se on helppolukuinen sekä koneille että ihmisille.

## How to:
Elmissä JSON:n käsittely tapahtuu `Json.Decode` ja `Json.Encode` moduulien avulla.

```Elm
import Json.Decode exposing (Decoder, string, int, field, decodeValue)
import Json.Encode exposing (Value, object, string, int)

type alias User =
    { name : String
    , age : Int
    }

-- JSON-dekooderi
userDecoder : Decoder User
userDecoder =
    field "name" string
        |> Json.Decode.andThen (\name ->
            field "age" int
                |> Json.Decode.map (\age ->
                    { name = name, age = age }
                )
           )

-- Esimerkki JSON-dekoodauksesta
userJson : String
userJson =
    "{\"name\":\"Esa\", \"age\":35}"

decodedUser : Result String User
decodedUser =
    decodeValue userDecoder (Json.Decode.string userJson)

-- JSON-enkooderi
encodeUser : User -> Value
encodeUser user =
    object
        [ ( "name", Json.Encode.string user.name )
        , ( "age", Json.Encode.int user.age )
        ]

-- Esimerkki JSON-enkoodauksesta
encodedUser : Value
encodedUser =
    encodeUser { name = "Esa", age = 35 }
```

## Deep Dive:
JSON-dekoodaus Elmissä voi tuntua monimutkaiselta verrattuna dynaamisten kielten kuten JavaScriptin tapaan käsitellä JSONia. Historiallisesti, Elm on kuitenkin valinnut tiukan tyypityksen reitin joka pakottaa kehittäjät käsittelemään dataa huolella. Tämä johtuu siitä, että Elm pyrkii välttämään runtime-virheitä. 

Vaihtoehtoina JSONin käsittelyyn Elm:ssä ovat custom-dekoodereiden kirjoittaminen tai käyttää `elm-json-decode-pipeline` pakettia vähentämään boilerplate-koodin määrää. 

JSON-enkoodaus on suoraviivaisempaa, koska enkooderin rakenne muistuttaa kirjaimellista JSON-objektia, mikä tekee siitä intuitiivisen Elm-ohjelmoijalle.

## See Also:
- Elm JSON-dekoodausdokumentaatio: [official Json.Decode documentation](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode)
- Elm JSON-enkoodausdokumentaatio: [official Json.Encode documentation](https://package.elm-lang.org/packages/elm/json/latest/Json-Encode)
- Esimerkki projektista: [Elm spa example](https://github.com/rtfeldman/elm-spa-example), jossa käytetään JSON-dekoodausta ja -enkoodausta.
- `elm-json-decode-pipeline`: [elm-json-decode-pipeline package](https://package.elm-lang.org/packages/NoRedInk/elm-json-decode-pipeline/latest/)
