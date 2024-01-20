---
title:                "Työskentely JSON:n kanssa"
html_title:           "Haskell: Työskentely JSON:n kanssa"
simple_title:         "Työskentely JSON:n kanssa"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

JSON eli JavaScript Object Notation on kevyt ja yleisesti käytössä oleva muoto serialisointiin, eli tiedon tallentamiseen tai välittämiseen. JSONin yksinkertainen syntaksi ja yhteensopivuus monien ohjelmointikielten kanssa tekee siitä suositun vaihtoehdon tietojen tallentamiseen ja välittämiseen ohjelmistoissa.

## Miten:

Haskellissa JSON-muotoista dataa voidaan käsitellä helposti käyttämällä Aeson-kirjastoa. Se sisältää valmiit funktiot JSON-muotoisen datan parsimiseen ja luomiseen, sekä työkalut sen käsittelemiseen.

Esimerkiksi, jos haluamme lukea JSON-tiedostosta tiedon ja tallentaa sen muuttujaan `data`, voimme tehdä seuraavaa:
```Haskell
import qualified Data.ByteString.Lazy as B
import Data.Aeson

main :: IO ()
main = do
  file <- B.readFile "tiedosto.json"
  let jsonData = decode file :: Maybe Value
  case jsonData of
    Nothing -> putStrLn "Tiedoston lukeminen epäonnistui!"
    Just data -> putStrLn "Tiedoston lukeminen onnistui!"
```

## Syvemmälle:

JSON-kielen kehitteli Douglas Crockford 2000-luvun alussa. Mistään uudesta ideasta ei ole kyse, sillä JSON on oikeastaan vain Crockfordin esittämä tapa merkitä JavaScript-olioita. Ennen JSON-muotoa käytettiin usein XML-tiedostoja, mutta XML:n raskas syntaksi tekee siitä hankalan käyttää tarkoituksiin, jotka eivät vaadi kompleksista rakennetta.

Haskelliin on myös olemassa muita vaihtoehtoja JSONin käsittelyyn, kuten Attoparsec- ja Parsec-kirjastot, jotka mahdollistavat JSONin käsittelyn parserin avulla. Aeson-kirjasto on kuitenkin suositeltu vaihtoehto, sillä se on käyttöönotoltaan helppokäyttöinen ja tehokas.

JSONin kyky käsitellä monenlaisia tietotyyppejä, kuten numeroita, merkkijonoja, olioita ja listoja, sekä sen yhteensopivuus monien ohjelmointikielten kanssa tekee siitä kätevän tiedon tallentamiseen ja välittämiseen. JSONia käytetään usein web-palveluissa, sekä selain-pohjaisissa sovelluksissa.

## Katso myös:

- [Aeson kirjaston virallinen dokumentaatio](https://hackage.haskell.org/package/aeson)
- [JSON käsittelemisestä Haskelliin ja muista kielistä](https://www.json.org/)