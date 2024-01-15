---
title:                "Työskentely jsonin kanssa"
html_title:           "Elm: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi JSON on oleellinen osa monia nykypäivän ohjelmointikieliä, myös Elm. JSON on helppo lukea ja kirjoittaa sekä samalla se on erittäin tehokas vaihtoehto tiedon tallentamiseen ja siirtämiseen. JSON:in käyttäminen auttaa myös selkeyttämään koodisi rakennetta ja antaa sinulle enemmän mahdollisuuksia muuttaa dataasi tarpeesi mukaan.

## Miten

JSON-tietorakenteen luominen Elm:ssä on erittäin yksinkertaista ja selkeää. Voit aloittaa luomalla uuden JSON-arvon käyttäen `Json.encode` funktiota, ja antamalla sille tarvittavat avaimet ja arvot. Alla on esimerkki:

```elm
import Json.Encode exposing (Value)

myJSON : Value
myJSON =
    Json.Encode.object
        [ ( "name", Json.encode "John" )
        , ( "age", Json.encode 25 )
        , ( "hobbies", Json.encodeArray [ Json.encode "skiing", Json.encode "reading", Json.encode "painting" ] )
        ]
```

Tässä esimerkissä luodaan JSON-arvo, joka sisältää nimen, iän ja harrastukset. Huomaa, kuinka jokainen arvo on ensin kääritty `Json.encode` funktion sisään. Tämä on tärkeää, jotta Elm ymmärtää, että kyseessä on JSON-arvo eikä mikään muu.

JSON-tietorakenteen purkaminen Elm:ssä tapahtuu käyttämällä `Json.Decode` moduulia. Voit tulkita JSON-arvon haluamallasi tavalla käyttämällä `Json.Decode` funktioita, kuten `Json.Decode.string`, `Json.Decode.int` ja `Json.Decode.list`. Alla on esimerkki:

```elm
import Json.Decode exposing (Value, string, int, list)

myName : Decode.Value -> String
myName json =
    case Json.Decode.decodeValue string json of
        Ok name ->
            name
        Err _ ->
            "Unknown"

myAge : Decode.Value -> Int
myAge json =
    case Json.Decode.decodeValue string json of
        Ok age ->
            age
        Err _ ->
            -1

myHobbies : Decode.Value -> List String
myHobbies json =
    case Json.Decode.decodeValue string json of
        Ok hobbies ->
            hobbies
        Err _ ->
            []

```

Tässä esimerkissä luodaan kolme erillistä funktiota, jotka purkavat JSON-arvon ja palauttavat tarvittavan datan haluttuun muotoon. Kuten huomaat, `Json.Decode.decodeValue` funktio ottaa parametreina `Json.Decode` moduulin funktion ja JSON-arvon. Jos purkaminen onnistuu, funktio palauttaa `Ok` arvon, muuten `Err` arvon.

## Syväsukellus

JSON:in kanssa työskentelyssä on tärkeää muistaa, että se on käytännössä vain merkkijonoina. Tämä tarkoittaa sitä, että kun lähetät tai vastaanotat JSON-tietorakenteita, ne on ensin muutettava merkkijonoiksi käyttämällä `Json.Encode.encode` tai `Json.Decode.decodeString` funktioita. Tämä Varmistaa, että tietosi säilyvät oikein ja että Elm ymmärtää ne oikein.

Elm tarjoaa myös monia hyödyllisiä työkaluja työskentelyyn JSON:in kanssa, kuten `Json.Decode` moduulin funktiot `map`, `andThen` ja `lazy`. Nämä auttavat työskentelemään JSON-tietorakenteen kanssa helpommin ja tehokkaammin.

## Katso myös

- [Elm: JSON dokumentaatio](https://package.elm-lang.org/packages/elm/json/latest/)
- [Codecademy: Learn JSON](https://www.codecademy.com/learn/