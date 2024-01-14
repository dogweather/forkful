---
title:                "Haskell: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi

Tervetuloa ensimmäiseen blogikirjoitukseeni, jossa tutustumme JSONiin Haskell-ohjelmointikielellä. JSON on erittäin hyödyllinen tiedonmuoto web-sovellusten ja pilvipalveluiden kehityksessä. Sen avulla voimme helposti vaihtaa tietoa eri järjestelmien välillä ja käsitellä monimutkaisempia tietorakenteita. Joten, jos olet kiinnostunut web-kehityksestä tai yleisesti informatiikan alasta, JSONin kanssa työskentely on ehdottomasti sinun tulee osata.

## Miten tehdä

Aloitetaan perusteista. Voit käyttää Haskellissa JSONin käsittelyyn `aeson`-kirjastoa, joka tarjoaa helpon tavan muuntaa JSON-tietorakenteita Haskellin omiksi tietotyypeiksi. Ensimmäisenä tarvitset `aeson` kirjaston tuontilausekkeen yläosaan:

```Haskell
import Data.Aeson
```

Nyt voimme luoda JSON-dataa esimerkiksi käyttämällä `object`-funktiota, joka luo sanakirjamaisen tietorakenteen avaimien ja arvojen pohjalta. Tässä esimerkissä luomme JSON-tietorakenteen, joka sisältää tiedot käyttäjän nimestä, iästä ja harrastuksista:

```Haskell
user :: Value
user = object [
  "name" .= "Matti",
  "age" .= 25,
  "hobbies" .= ["luistelu", "matkailu", "lukeminen"]
]
```

Huomaa, että käytämme `.`-operaattoria lisätäksemme avaimen ja `.= `-operaattoria asettaaksemme arvon. Tämä on yleinen käytäntö `aeson`-kirjastossa JSONin käsittelyssä.

Nyt kun meillä on JSON-tieto, voimme muuntaa sen Haskellin omaksi tietotyypiksi `decode`-funktion avulla. Tässä tapauksessa `User`-niminen tietotyyppi:

```Haskell
data User = User {
  name :: String,
  age :: Int,
  hobbies :: [String]
} deriving (Show, Generic)

instance FromJSON User

main :: IO ()
main = do
  let maybeUser = decode user :: Maybe User
  print maybeUser
  -- Output: Just (User {name = "Matti", age = 25, hobbies = ["luistelu", "matkailu", "lukeminen"]})
```

Nyt `maybeUser` on Haskellin `Maybe`-tyyppi, joka voi olla joko `Just User` tai `Nothing`. Kannattaa aina tarkistaa `Nothing`-tapaus, jos tietorakenne ei olisi oikeassa muodossa.

## Syväsukellus

On myös tärkeää huomata, että Haskellissa voimme muuntaa `User`-tietotyypin takaisin JSON-tietorakenteeksi käyttämällä `encode`-funktiota. Tässä on esimerkki:

```Haskell
encodedUser :: ByteString
encodedUser = encode maybeUser
  -- Output: "{\"name\":\"Matti\",\"age\":25,\"hobbies\":[\"luistelu\",\"matkailu\",\"lukeminen\"]}"
```

Ilman `Maybe`-tyyppiä voimme myös käyttää `eitherDecode`-funktiota, joka palauttaa joko oikean tietotyypin tai virheen:

```Haskell
eitherUser :: Either String User
eitherUser = eitherDecode encodedUser
```

Kuten näet, JSONin käsittely Haskellissa on helppoa ja tehokasta `aeson`-kirjaston avulla. Muista kuitenkin aina tarkistaa virhetapaukset, jotta koodisi olisi turvallista ja robust