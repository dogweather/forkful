---
title:    "Haskell: Tekstitiedoston lukeminen"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Useimmat meistä ovat luultavasti törmänneet tekstifileihin aikaisemmin työskennellessämme data-analyysin tai ohjelmoinnin parissa. Kyseessä on yksi yleisimmistä tiedostotyypeistä, joten on tärkeää tietää, kuinka niitä voidaan käsitellä tehokkaasti ja kätevästi. Tässä blogitekstissä opimme, kuinka lukea tekstifilejä Haskellilla ja miksi se voi olla hyödyllistä.

## Kuinka tehdä se

Lukeminen tekstifilejä Haskellilla on suhteellisen yksinkertaista. Siitä huolimatta se voi joskus olla hieman hämmentävää, etenkin jos olet juuri aloittamassa Haskell-ohjelmointia. Tässä on kuitenkin muutama esimerkki, joista voit ottaa mallia:

```Haskell
import System.IO

main = do
  file <- openFile "demo.txt" ReadMode  -- avataan tiedosto lukutilassa
  contents <- hGetContents file         -- luetaan tiedoston sisältö
  putStr contents                       -- tulostetaan sisältö konsoliin
```

Tässä esimerkissä käytämme `System.IO` -paketin `openFile` -funktiota avataksemme tiedoston nimeltä "demo.txt" lukutilassa. Avatessamme tiedoston käytämme `hGetContents` -funktiota lukemaan sen sisällön. Lopuksi käytämme `putStr` -funktiota tulostamaan tiedoston sisällön konsoliin.

Voit myös halutessasi käyttää `withFile` -funktiota, joka avaa tiedoston, suorittaa annetun toiminnon ja sulkee tiedoston automaattisesti. Se näyttää tältä:

```Haskell
import System.IO

main = do
  withFile "demo.txt" ReadMode (\handle -> do
    contents <- hGetContents handle -- luetaan tiedoston sisältö
    putStr contents                 -- tulostetaan sisältö konsoliin
  )
```

Molemmat esimerkit toimivat samalla tavalla ja antavat saman tulosteen. Voit kokeilla muuttaa koodia, esimerkiksi lukea tiedoston rivit ja käsitellä niitä erikseen.

## Syventävä sukellus

Nyt kun olemme käsitelleet perusteet, haluan nostaa esille muutamia lisäaspekteja, jotka voivat auttaa sinua lukiessasi ja työskennellessäsi tekstifileiden kanssa Haskellilla.

Ensinnäkin, sinun tulisi aina sulkea tiedosto, kun olet saanut sen käsiteltyä loppuun. Tämä on tärkeää, sillä avoimet tiedostot voivat aiheuttaa muistiongelmia tai jopa hidastaa ohjelmaa. Voit sulkea tiedoston `hClose` -funktiolla.

Toiseksi, voit käyttää `hIsEOF` -funktiota tarkistaaksesi, oletko jo päässyt tiedoston loppuun. Tämä voi auttaa sinua käsittelyssä, esimerkiksi jos haluat käsitellä tiedostoa rivillä kerrallaan.

Lisäksi voit kokeilla tutkia `Text` -paketin tarjoamia työkaluja tekstien käsittelyyn, kuten `lines` ja `unlines` -funktioita, jotka jakavat tai yhdistävät tekstin rivien avulla.

Lopuksi haluan muistuttaa, että muuttujien tyypin määrittäminen voi auttaa sinua lukiessasi tiedostoa, etenkin jos se sisältää monen tyyppistä dataa. Esimerkiksi jos haluat lukea tiedoston kokonaislukuja, voit käyttää