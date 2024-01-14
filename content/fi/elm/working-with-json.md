---
title:                "Elm: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi

Elm on ohjelmointikieli, joka on suunniteltu luomaan modernia ja skaalautuvaa web-kehitystä. JSON (JavaScript Object Notation) on yksi yleisimmin käytetyistä formaateista siirtää tietoa web-sovellusten ja palvelinten välillä. Elm tarjoaa helpon tavan työskennellä JSONin kanssa, mikä tekee siitä houkuttelevan kielen kehittäjille, jotka haluavat työskennellä moderneissa web-sovelluksissa.

## Miten

Elm tarjoaa sisäänrakennetun JSON.Decoder ja JSON.Encoder- moduulit, jotka mahdollistavat helpon koodin kirjoittamisen JSON-tietojen muuntamiseksi Elm-tietotyypiksi ja päinvastoin.

```Elm
import Json.Decode exposing (decodeValue, int, string)
import Json.Encode exposing (encode, int, list, object, string)

-- Luodaan JSON-pituusmuuntelu
jsonString : String
jsonString =
  """
  {
    "nimi": "Matti",
    "ika": 32
  }
  """

-- Määritellään henkilö-tyyppi
type alias Henkilo =
  { nimi : String
  , ika : Int
  }

-- Luo JSON:in dekooderi
henkiloDecoder : Decode.Decoder Henkilo
henkiloDecoder =
  Decode.map2 Henkilo
    (Decode.field "nimi" string)
    (Decode.field "ika" int)

-- Muunna JSON Elm tietotyypiksi
muunnaJSON : Result String Henkilo
muunnaJSON =
  jsonString
    |> decodeValue henkiloDecoder

-- Toista sama JSON Encoder-painikkeella
-- Luo Henkilo-tietorakenne
person : Henkilo
person =
  Henkilo "Matti" 32

-- Muunna Elm-tietotyyppi JSON:iksi
tallennaJSON : Encode.Value
tallennaJSON =
  person
    |> encode
```

Tässä esimerkissä luodaan yksinkertainen JSON-tiedosto, joka sisältää nimen ja iän, ja muunnetaan se Elm-tietotyypiksi käyttämällä JSON.Decoder ja JSON.Encoder- moduuleja. Tämän jälkeen yksinkertaisesti muunnetaan Elm-tietotyyppi takaisin JSON-muotoon TallennaJSON-funktiolla.

Tuloste:

```Elm
Ok { nimi = "Matti", ika = 32 }
```

Tästä nähdään, että JSON on onnistuneesti muunnettu Elm-tietotyypiksi, ja sitä voidaan käyttää edelleen ohjelmassa.

## Syväkellunta

JSON:n kanssa työskentely Elmissä on helppoa ja tehokasta. JSON.Decoder ja JSON.Encoder- moduulit tarjoavat paljon hyödyllisiä toimintoja, kuten map ja field, jotka auttavat muuntamaan JSON-dataa eri tietotyypeiksi. Lisäksi voit käyttää many-valintoja olemaan sanelematta tarkkoja vastaavuuksia esimerkiksi, jos ulkoisten palvelujen tiedot ovat epätäydellisiä.

Elm tarjoaa myös mahdollisuuden yhdistää JSON-tiedostoja yhdessä. Tämä on hyödyllistä, jos sinun on käsiteltävä useita eri JSON-tiedostoja ja yhdistettävä ne yhdeksi tietotyypiksi. Tätä varten voit käyttää Json.Decode.at- funktiota. Se ottaa merkkijonon parametrina ja palauttaa decoderin, joka luo esiintymisarvon halutulle alueelle.

## Katso myös

- [Elm dokumentaatio JSON: lle] (https://guide.elm-lang.org/effects/json.html)
- [JSON.Decoder Moduuli] (https://package.elm-lang.org/packages/elm/json/latest/