---
title:                "Haskell: Yhteistyössä yaml:n kanssa"
simple_title:         "Yhteistyössä yaml:n kanssa"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi?

Kirjoittaessasi Haskell-ohjelmia, saatat törmätä YAML-tiedostojen käsittelyyn. YAML on käyttäjäystävällinen tietojen esitysmuoto, joka on suosittu erilaisten tietojen tallentamiseen ja siirtämiseen ohjelmoinnissa. Tässä artikkelissa kerromme, miksi YAML on hyödyllinen ja miten sitä käytetään Haskellissa.

## Miten?

Haskellilla on jo olemassa oleva paketti nimeltä "yaml", joka tarjoaa hyvän rajapinnan YAML-tiedostojen käsittelyyn. Voimme aloittaa käyttämällä sitä asentamalla sen Stack-komennon avulla:

```Haskell
stack install yaml
```

Kun se on asennettu, voimme tuoda sen projektiimme käyttämällä seuraavaa koodia:

```Haskell
import qualified Data.Yaml as Yaml
import Data.ByteString (readFile)
```

Nyt voimme lukea YAML-tiedostoja käyttämällä `Yaml.decodeEither` -funktiota ja antamalla sille tiedoston polun sekä halutun datatyypin:

```Haskell
main :: IO ()
main = do
  res <- Yaml.decodeEither <$> readFile "esimerkki.yaml"
  case res of
    Left e -> putStrLn $ "Virhe: " ++ e
    Right arvot -> print (arvot :: [String])
```

Tässä esimerkissä luemme YAML-tiedoston, joka sisältää listan stringejä, ja tulostamme ne konsoliin. Voimme myös muuttaa datatyypin esimerkiksi `Map String Int` tai `Maybe Bool`, jos tiedostossa on vastaavat arvot. Kokeile vaikka itse!

## Syvemmälle

YAML-tiedostot ovat hierarkkisia, mikä tarkoittaa että ne voivat sisältää manyyym mutation tasoisia objekteja ja arvoja. Tämän takia `Yaml.decodeEither`-funktion palauttama tyyppi on `Either String Value`, missä `Value` sisältää `ValueScalar`, `ValueSequence` ja `ValueMapping` -tyyppejä erilaisia arvoja varten. Yksityiskohtaisemman dokumentaation löydät `yaml`-paketista.

Seuraavana haasteena on muuntaa `Value` erilaisiin datatyyppeihin. Tätä varten voit käyttää esimerkiksi `Control.Lens`-nimitystä tai `Data.Aeson`-pakettia, jotka tarjoavat työkaluja tämän tekemiseen. On myös hyödyllistä lukea ja tutkia erilaisia esimerkkejä `yaml`-paketista ja muista lähteistä, jotka auttavat ymmärtämään YAML-tiedostojen käsittelyä Haskellilla paremmin.

## Katso myös

- `yaml`-paketti Hackagesta: <https://hackage.haskell.org/package/yaml>
- Hyvä esimerkki YAML-tiedoston käsittelystä Haskellilla: <https://markkarpov.com/tutorial/yaml.html>
- Manipulointi YAML-tiedostoilla käyttäen `Control.Lens`-paketin apufunktioita: <https://www.snoyman.com/blog/2016/10/conduit-sorted-yaml-parser>
- Omahyvä esimerkkiprojekti, joka käyttää YAML-tiedostoja pienoissääsovellukseen: <https://github.com/andorp/meteo-hs>