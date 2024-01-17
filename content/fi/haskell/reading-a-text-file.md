---
title:                "Tiedostojen lukeminen"
html_title:           "Haskell: Tiedostojen lukeminen"
simple_title:         "Tiedostojen lukeminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Tiedoston lukeminen on prosessi, jossa ohjelma lukee datan sisältämän tiedoston ja tallentaa sen muistiin. Tämä on tärkeää, sillä tiedostot ovat yleinen tapa tallentaa tietoa ja ohjelmien on usein käsiteltävä näitä tietoja. Siksi tiedostojen lukeminen on tärkeä taito kaikille ohjelmoijille.

## Miten:
Tiedoston lukeminen Haskellissa voi tapahtua monella eri tavalla, mutta yleisin tapa on käyttää ```getContents``` funktiota, joka lukee tiedoston sisällön ja palauttaa sen merkkijonona. Tämän jälkeen voit käsitellä tiedoston sisältöä miten haluat, esimerkiksi tulostaa sen näytölle.

Esimerkki koodi:

```Haskell
import System.IO

main = do
  fileContents <- getContents
  putStrLn fileContents
```

Tämä koodi lukee tiedoston sisällön ja tulostaa sen näytölle.

Osassa "Deep Dive" voit tutustua muihin tapoihin lukea tiedostoja Haskellissa ja tutkia syvemmin tiedoston käsittelyn yksityiskohtia.

## Deep Dive:
Haskell tarjoaa monia erilaisia tapoja lukea tiedostoja, kuten ```readFile``` ja ```withFile```. Voit myös käyttää lisäpakettia ```Data.ByteString``` tehokkaampaa tiedostonlukua varten.

On myös tärkeää mainita, että vaikka tiedoston lukeminen on yleinen tapa, se ei aina ole paras vaihtoehto. Joskus on parempi tallentaa data tietokantaan tai käyttää APIa hakeaksesi tiedot suoraan ilman tallentamista.

Tässä osassa voit myös tutustua tiedostonmuotoihin ja niiden erilaisiin käsittelytapoihin.

## See Also:
Vertaisitkesi tiedoston lukemista muihin tapoihin käsitellä dataa, kuten tietokantoihin tai integraatioihin APIen kanssa. Voit myös tutkia lisää ```System.IO``` paketista ja sen tarjoamista tiedostonkäsittelytoiminnoista.

Lue lisää Haskellin tiedostonkäsittelystä täältä: [tiedostonkäsittely Haskellissa](https://www.haskell.org/tutorial/io.html)