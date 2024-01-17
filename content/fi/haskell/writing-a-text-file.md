---
title:                "Tiedoston kirjoittaminen"
html_title:           "Haskell: Tiedoston kirjoittaminen"
simple_title:         "Tiedoston kirjoittaminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Tekstitiedoston kirjoittaminen tarkoittaa käytännössä sitä, että tallennamme tekstiä tiedostoon. Tämä voi olla hyödyllistä esimerkiksi silloin, kun haluamme tallentaa tiettyjä tietoja ohjelmamme käytön aikana tai lukea tietoja myöhemmin ohjelmaamme.
Ohjelmoijat tekevät tekstintiedostojen kirjoittamista useista eri syistä. Se voi olla osa ohjelman toiminnallisuutta, esimerkiksi tietojen tallentaminen tiettyyn muotoon. Se voi myös olla tapa kommunikoida muiden ohjelmien kanssa tai tallentaa tärkeitä tietoja, kuten ohjelman asetuksia.

## Näin teet sen:
Koodiesimerkit ja tulostukset seuraavat ```Haskell... ``` koodilohkoissa.

```Haskell
import System.IO

main = do
  writeFile "tekstitiedosto.txt" "Tässä on esimerkki tekstistä!"
```

Tämä koodi luo uuden tekstitiedoston nimeltä "tekstitiedosto.txt" ja tallentaa siihen tekstin "Tässä on esimerkki tekstistä!".

```Haskell
import System.IO

main = do
  appendFile "tekstitiedosto.txt" "\nTämä on toinen rivi."
```

Tämä koodi lisää tekstitiedostoon jo olevan tekstin jatkoksi toisen rivin, jonka sisältönä on "Tämä on toinen rivi."

## Syvemmälle:
Tekstin tallentaminen tiedostoon on ollut aina tärkeä osa ohjelmointia. Se on yksi tapa tallentaa pysyvästi tietoja ja kommunikoida muiden ohjelmien kanssa. On myös muita tapoja tallentaa tietoja, kuten tietokantoihin tai muistiin, mutta tekstitiedostot ovat yleensä yksinkertaisin ja helpoin tapa tallentaa tietoja.

Haskellin ```System.IO``` -kirjasto tarjoaa erilaisia ​​funktioita tekstien kirjoittamiseen ja lukemiseen tiedostoista. Voit myös käyttää muita kirjastoja, kuten ```Data.Text```, tekstin manipulointiin ja tallentamiseen tiedostoihin. Voit myös käyttää erilaisia ​​työkaluja, kuten Tiukasti-kirjastoa, hallitsemaan tekstiä ja varmistamaan, että se tallennetaan tiedostoon oikeassa muodossa.

## Katso myös:
Voit lukea lisätietoja tekstitiedoston kirjoittamisesta Haskellilla täältä: https://wiki.haskell.org/How_to_read_and_write_a_file

Täällä on myös loistava opas Haskellin lukemiseen ja kirjoittamiseen tekstitiedostoihin: https://stackabuse.com/reading-and-writing-files-in-haskell/