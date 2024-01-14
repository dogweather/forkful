---
title:                "Elm: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## Miksi
Elm on ohjelmointikieli, joka on tullut suosituksi sen yksinkertaisuuden ja vahvan tyypityksen ansiosta. Sen avulla voit luoda kauniita ja toimivia web-sovelluksia helposti ja tehokkaasti. CSV-tiedostot ovat yleinen tapa tallentaa ja jakaa taulukkomuotoista dataa. Elm tarjoaa kätevän tavan työskennellä CSV-tiedostojen kanssa, mikä voi säästää aikaa ja vaivaa.

## Kuinka tehdä
CSV-tiedostojen käsittely Elmillä on helppoa ja vaivatonta, kiitos Elm CSV -kirjaston. Alla on koodinäyte, joka näyttää, kuinka voit ladata ja käsitellä CSV-tiedoston Elm-sovelluksessa:

```Elm
import CSV exposing (load, parse)
import Http 
import Json.Decode exposing (decodeString, field, list, string)

type alias Person = 
    { name : String
    , age : Int
    }

personDecoder : Json.Decoder Person
personDecoder = 
    Json.Decode.map2 Person
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "age" Json.Decode.int)

main : Program () 
main = 
    Http.get 
        { url = "example.csv"
        , expect = Http.expectString 
            (Decode.map personDecoder 
            (Csv.parse ";") 
        }
```

Yllä oleva koodinäyte lataa CSV-tiedoston nimeltä "example.csv", jossa on puolipisteellä erotetut tiedot. Sen jälkeen se parsii tiedot ja muuntaa ne listaksi henkilöitä, jotka sisältävät nimen ja iän. Voit käsitellä tätä tietoa haluamallasi tavalla, esimerkiksi näyttämällä sen käyttäjälle.

```
lista henkilöitä : 
[ 
    { name = "Matti", age = 25 } 
    { name = "Hanna", age = 32 }
    { name = "Teemu", age = 19 }
] 
```

## Syvennystä
Elm CSV -kirjasto tarjoaa myös muita toimintoja, kuten CSV-tiedostojen tallentamisen ja muuntamisen JSON-muotoon. Voit tutustua tarkemmin kirjaston dokumentaatioon ja löytää muita hyödyllisiä toimintoja CSV-tiedostojen käsittelyyn.

### Katso myös
- [Elm CSV kirjaston dokumentaatio](https://package.elm-lang.org/packages/NoRedInk/elm-csv/latest/)
- [Elm ohjelmointikielen verkkosivut](https://elm-lang.org/)
- [JSON-muoto](https://json.org/)