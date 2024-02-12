---
title:                "Työskentely YAML:n kanssa"
aliases: - /fi/elm/working-with-yaml.md
date:                  2024-02-03T19:25:37.898661-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskentely YAML:n kanssa"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?
Elm ei tarjoa sisäänrakennettua tukea YAML:lle, joka on datan serialisointiformaatti ja jota käytetään usein konfiguraatiotiedostoissa tai datan jakamisessa, koska se painottaa vahvasti tyypin turvallisuutta ja ennustettavia tuloksia. Ohjelmoijat kohtaavat kuitenkin usein YAML:ää käsitellessään API:ja tai konfiguraatioita web-kehityksessä, mikä edellyttää luotettavia menetelmiä YAML-datan jäsentämiseksi Elmin tiukan tyypitettyyn ekosysteemiin saumattoman integraation ja manipuloinnin mahdollistamiseksi.

## Kuinka:
Käsitelläksesi YAML:ää Elm:ssä, sinun tarvitsee yleensä muuntaa YAML JSON:ksi Elm:n ulkopuolella ja sitten käyttää Elmin sisäänrakennettua JSON-purausfunktionaalisuutta datan käsittelyyn. Vaikka tämä lähestymistapa vaatii ylimääräisen muunnosvaiheen, se hyödyntää Elmin vahvaa tyypitysjärjestelmää datan eheyden varmistamiseen. Suosittuja työkaluja YAML:sta JSON:ksi muuntamiseen sisältävät online-muuntimet tai taustapalvelut. Kun sinulla on JSON, voit käyttää Elmin `Json.Decode` moduulia datan käsittelyyn.

Ensiksi, oletetaan, että sinulla on seuraava YAML-data:

```yaml
person:
  name: Jane Doe
  age: 30
```

Muunna se JSON-muotoon:

```json
{
  "person": {
    "name": "Jane Doe",
    "age": 30
  }
}
```

Seuraavaksi, määrittele Elm-mallisi ja purkajasi:

```elm
module Main exposing (..)

import Html exposing (text)
import Json.Decode as Decode

type alias Person =
    { name : String
    , age : Int
    }

personDecoder : Decode.Decoder Person
personDecoder =
    Decode.map2 Person
        (Decode.field "name" Decode.string)
        (Decode.field "age" Decode.int)

```

Käyttääksesi tätä purkajaa JSON:sta Elm-tyyppiin:

```elm
import Json.Decode as Decode

jsonString = 
    """
    {
      "person": {
        "name": "Jane Doe",
        "age": 30
      }
    }
    """

decodeResult = Decode.decodeString (Decode.field "person" personDecoder) jsonString

main =
    case decodeResult of
        Ok person ->
            Html.text ("Hei, " ++ person.name ++ "!")
            
        Err _ ->
            Html.text "Virhe tapahtui purkaessa."
```

Tulostus (renderoituna Elm-sovelluksessa):
```
Hei, Jane Doe!
```

Tämä lähestymistapa varmistaa, että voit työskennellä YAML-datan kanssa Elm:ssä hyödyntämällä JSON:ia väliformaattina, käyttäen hyväksi Elmin vahvaa tyypitysjärjestelmää ja JSON-purauskyvykkyyksiä turvallisesti ja tehokkaasti ulkoisen datan käsittelyyn.
