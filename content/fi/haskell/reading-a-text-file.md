---
title:                "Tekstitiedoston lukeminen"
html_title:           "Haskell: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit lukea tekstiedoston Haskell-ohjelmalla? Se voi olla hyödyllistä, kun haluat käsitellä isoa määrää tekstimuotoista dataa, kuten logitiedostoja tai CSV-tiedostoja.

## Miten

Lukeminen ja käsittely Haskellilla on helppoa ja tehokasta, kiitos sen monipuolisten kirjastojen ja sisäänrakennettujen toimintojen. Voit käyttää ```readFile``` -funktiota ladataksesi tiedoston ja ```lines``` -funktiota jakamaan sen riveihin. Alla on yksinkertainen esimerkki:

```Haskell
import System.IO

main = do
  file <- readFile "tiedosto.txt"
  let rivit = lines file
  print rivit
```

Tämä koodi lukee "tiedosto.txt" -tiedoston ja tallentaa sen sisällön listana, jossa jokainen alkio on yksi tiedoston rivi. Voit sitten käsitellä tätä dataa haluamallasi tavalla, kuten tulostaa tietyn rivin tai laskea rivien määrän.

## Syväsukellus

```readFile``` ja ```lines``` ovat vain kaksi esimerkkiä Haskellin tarjoamista monipuolisista toiminnoista tekstitiedostojen lukemiseen ja käsittelyyn. Voit myös käyttää muita funktioita, kuten ```words``` ja ```unlines``` tuettaessasi tekstin jakamista ja yhdistämistä. Voit myös hyödyntää erilaisia kirjastoja, kuten ```Data.Text``` tai ```Data.ByteString``` jos haluat käsitellä tekstitiedostoja eri formaateissa.

## Katso myös

- [Haskellin viralliset dokumentaatiot](https://www.haskell.org/documentation/)
- [Hackage: Haskellin pakettikirjasto](https://hackage.haskell.org/)
- [Haskell.org:n oppimismateriaalit](https://www.haskell.org/learn/)
- [Learn You a Haskell -kirja](http://learnyouahaskell.com/)