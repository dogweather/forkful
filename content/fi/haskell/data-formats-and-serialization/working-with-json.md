---
title:                "Työskentely JSON:n kanssa"
aliases:
- /fi/haskell/working-with-json.md
date:                  2024-02-03T19:23:41.947965-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskentely JSON:n kanssa"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
JSONin (JavaScript Object Notation) käsittely Haskellissa sisältää JSON-datan jäsentämisen Haskell-tyypeiksi ja Haskell-tyyppien muuntamisen takaisin JSONiksi. Ohjelmoijat tekevät näin, jotta heidän Haskell-sovelluksensa voivat vaihtaa tietoja verkkopalveluiden tai API:en kanssa saumattomasti, mikä on nykyaikaisessa ohjelmistokehityksessä yleinen käytäntö alustojen välisessä datan vaihdossa.

## Miten:
Haskellissa ei ole sisäänrakennettua tukea JSONille kuten JavaScriptissä, mutta kolmannen osapuolen kirjastojen, kuten **Aesonin**, avulla JSONin käsittely muuttuu suoraviivaiseksi. Aeson tarjoaa sekä korkean tason että matalan tason funktioita koodaukseen (Haskell-arvojen muuntaminen JSONiksi) ja dekoodaukseen (JSONin jäsentäminen Haskell-arvoiksi).

### Aesonin asentaminen
Lisää ensin Aeson projektisi riippuvuuksiin päivittämällä `.cabal`-tiedostosi tai käyttämällä Stackia tai Cabalia suoraan:

```shell
cabal update && cabal install aeson
```
tai, jos käytät Stackia:
```shell
stack install aeson
```

### JSONin jäsentäminen
Aloitetaan perusesimerkillä, jossa dekoodataan JSON-data Haskell-tyypiksi. Oletetaan, että meillä on seuraava JSON henkilöstä:

```json
{
  "name": "John Doe",
  "age": 30
}
```

Määritä ensin vastaava Haskell-datatyypi ja tee siitä `FromJSON`-instanssi:

```haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy as B

data Henkilo = Henkilo
  { nimi :: String
  , ika :: Int
  } deriving (Generic, Show)

instance FromJSON Henkilo

-- Funktio JSONin dekoodaamiseksi tiedostosta
dekoodaaHenkilo :: FilePath -> IO (Maybe Henkilo)
dekoodaaHenkilo tiedostopolku = do
  henkiloJson <- B.readFile tiedostopolku
  return $ decode henkiloJson
```
Käyttö:
Olettaen, että `person.json` sisältää yllä näytetyn JSON-datan, suorita:
```haskell
main :: IO ()
main = do
  maybeHenkilo <- dekoodaaHenkilo "person.json"
  print maybeHenkilo
```
Esimerkkituloste:
```haskell
Just (Henkilo {nimi = "John Doe", ika = 30})
```

### Haskell-arvojen koodaus JSONiksi
Muuntaaksesi Haskell-arvon takaisin JSONiksi, sinun on tehtävä tyypistäsi `ToJSON`-instanssi ja sitten käytettävä `encode`-funktiota.

```haskell
import Data.Aeson (ToJSON, encode)
import GHC.Generics (Generic)

-- Olettaen aikaisemmin määritellyn Henkilo-tyypin

instance ToJSON Henkilo

koodaaHenkilo :: Henkilo -> B.ByteString
koodaaHenkilo = encode

main :: IO ()
main = do
  let henkilo = Henkilo "Jane Doe" 32
  putStrLn $ show $ koodaaHenkilo henkilo
```
Esimerkkituloste:
```json
{"nimi":"Jane Doe","ika":32}
```

Nämä esimerkit demonstroivat JSONin peruskäsittelyä Haskellissa käyttäen Aesonia. Muistathan, että Aeson tarjoaa paljon muutakin, mukaan lukien mukautetut jäsentelysäännöt, monimutkaisen sisäkkäisen JSONin käsittelyn ja paljon muuta, mikä soveltuu erilaisiin tarpeisiin ja skenaarioihin.
