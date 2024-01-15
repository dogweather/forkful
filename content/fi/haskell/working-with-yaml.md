---
title:                "Työskentely yaml:n kanssa"
html_title:           "Haskell: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi

Jos olet kiinnostunut ohjelmoinnista, luultavasti olet törmännyt YAML:iin. YAML (Yet Another Markup Language) on formaatti, jota käytetään tiedostojen tallentamiseen ja siirtämiseen tietokoneiden välillä. Se on erityisen hyödyllinen, kun halutaan tallentaa tai vaihtaa tietoa rakenteisessa muodossa, kuten taulukkoina tai joukkoina. Jos olet kiinnostunut datan hallinnasta ja manipuloinnista, YAML on loistava väline tähän tarkoitukseen.

## Näin aloitat

Aloittaaksesi YAML:n käytön Haskellissa, sinun tulee ensin asentaa paketti nimeltä "yaml". Voit tehdä tämän suorittamalla seuraavan komennon GHCi-konsolissa:

```
cabal install yaml
```

Tämän jälkeen sinun tulee tuoda YAML-paketti moduuliesi joukkoon:

```
import Data.YAML
```

Nyt voit käyttää YAML:ia koodissasi. Annetaan esimerkiksi olemassa oleva YAML-tiedosto nimeltä "mydata.yaml" jo sisältää seuraavanlaista dataa:

```
- title: "Esimerkki"
  author: "Maija Meikäläinen"
  year: 2020
- title: "Toinen esimerkki"
  author: "Matti Mallikas"
  year: 2019
```

Voimme käyttää YAML-paketin funktiota "decodeFileThrow" lukeaksemme tämän tiedoston sisällön ja tallentaaksemme sen muuttujaan koodissamme:

```
mydata <- decodeFileThrow "mydata.yaml" :: IO [YAML]
```

Tämän jälkeen voimme käyttää muuttujaa "mydata" saadaksemme tiedoston sisällön koodissa. Esimerkiksi voimme tulostaa tiedoston sisällön näytölle seuraavalla tavalla:

```
print $ show mydata
```

Ja tuloste olisi:

```
[Node (Mapping [Scalar (Str "title"),Scalar (Str "Esimerkki"),Scalar (Str "author"),Scalar (Str "Maija Meikäläinen"),Scalar (Str "year"),Scalar (Int 2020)]),Node (Mapping [Scalar (Str "title"),Scalar (Str "Toinen esimerkki"),Scalar (Str "author"),Scalar (Str "Matti Mallikas"),Scalar (Str "year"),Scalar (Int 2019)])]
```

## Syväsukellus

Kun olet perehtynyt YAML:n perusominaisuuksiin ja haluat syventää tietämystäsi, voit tutkia erilaisia tapoja käyttää sitä kanssa Haskellissa. Voit esimerkiksi luoda omaa dataa ja tallentaa sen YAML-tiedostoon käyttämällä "encodeFile" funktiota. Voit myös käyttää YAML:ia yhdessä muiden pakettien kanssa, kuten "aeson", joka mahdollistaa JSON-tiedostojen käsittelyn sekä JSON:sta YAML:ksi muuntamisen.

## Katso myös

- [YAML Specification](https://yaml.org/spec/1.2/spec.html)
- [YAML-haskell paketin dokumentaatio](https://hackage.haskell.org/package/yaml)
- [Haskellin peruskurssi](https://www.cis.upenn.edu/~cis194/spring13/)