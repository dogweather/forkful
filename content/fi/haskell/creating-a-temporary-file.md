---
title:                "Väliaikaisen tiedoston luominen"
html_title:           "Haskell: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Miksi: Miksi luoda väliaikainen tiedosto?

Väliaikaisen tiedoston luominen on hyödyllistä silloin, kun tarvitaan väliaikaista tallennustilaa ohjelman suorituksen aikana. Se voi auttaa välttämään turhia tallennuksia pysyviin tiedostoihin ja mahdollistaa ohjelman suorituksen jälkeen tiedoston poistamisen varmistaen, ettei turhia tiedostoja jää jäljelle.

## Kuinka: Väliaikaisen tiedoston luominen

Väliaikaisen tiedoston luominen Haskellissa on helppoa käyttämällä 'System.IO.Temp' -kirjastoa. Esimerkiksi, seuraava koodi luo väliaikaisen tiedoston nimellä "temp.txt" ja kirjoittaa siihen tekstin "Tämä on väliaikainen tiedosto.":

```Haskell
import System.IO.Temp

main = do
    withSystemTempFile "temp.txt" $ \path handle -> do
        hPutStrLn handle "Tämä on väliaikainen tiedosto."
```

Mikäli haluat käyttää tiedostoa muissa funktioissa, voit sen sijaan käyttää 'withTempFile' -funktiota, joka palauttaa tiedoston polun. Esimerkiksi:

```Haskell
import System.IO.Temp

main = do
    tmpFilePath <- withTempFile "temp.txt" $ \path handle ->
        return path
    putStrLn $ "Väliaikainen tiedosto luotu polkuun: " ++ tmpFilePath
```

Output: Väliaikainen tiedosto luotu polkuun: /tmp/tmp11322.txt

## Deep Dive: Väliaikaisen tiedoston luomisen mekanismi

Väliaikaisen tiedoston luominen 'System.IO.Temp' -kirjaston avulla tapahtuu seuraavalla tavalla:

1. Kirjasto luo väliaikaisen hakemiston käyttäen järjestelmän 'tmp' hakemistoa.
2. Tiedoston nimi ja polku generoidaan käyttäen satunnaisgeneraattoria.
3. Tiedosto avataan ja käyttäjän antama koodi suoritetaan.
4. Ohjelman suorituksen jälkeen tiedosto poistetaan ja hakemisto tyhjennetään.

Väliaikaisen tiedoston luominen on siis turvallinen tapa käyttää väliaikaista tallennustilaa ohjelmassa.

## Katso myös

- https://hackage.haskell.org/package/temporary - Toinen kirjasto väliaikaisen tiedoston luomiseen Haskellissa.
- https://www.haskell.org/ - Virallinen Haskellin sivusto, jonka kautta voit löytää paljon lisätietoa ja resursseja kieleen liittyen.