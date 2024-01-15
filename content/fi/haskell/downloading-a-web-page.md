---
title:                "Lataaminen verkkosivulle"
html_title:           "Haskell: Lataaminen verkkosivulle"
simple_title:         "Lataaminen verkkosivulle"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi

Jokainen meistä on varmasti joskus tarvinnut ladata muutaman verkkosivun datan omalle tietokoneelle. Ehkä halusit tallentaa artikkelin offline-luvuksi tai tutkia tarkemmin sivun rakennetta ja sisältöä. Oli syy mikä tahansa, Haskellilla tämä tehtävä on helppo toteuttaa ja osoittaa jälleen kerran kielen monipuolisuuden.

## Miten

Käytännössä kaikki verkkosivut toimivat HTTP-protokollan avulla, joten tarvitsemme vain pienen kirjaston, joka osaa kommunikoida tietokoneesi ja verkkosivun välillä. Tämä kirjasto on `wget` ja voimme ladata sen komentoriviltä seuraavalla komennolla:

```Haskell
cabal install wget
```

Tämän jälkeen voimme aloittaa lataamisen suoraan Haskellin GHCI-tulkista. Otetaan esimerkiksi lataus Google-hakukoneen etusivulta:

```Haskell
import Data.ByteString (concat)
import Network.Wreq (get, responseBody)

main :: IO ()
main = do
  res <- get "https://www.google.com/" -- Lataa sivun
  let body = response body res -- Tallentaa sivun sisällön
      file = "google.html" -- Tiedoston nimi
  Data.ByteString.writeFile file body -- Tallentaa tiedoston
```

Tämä esimerkki lataa Google-hakukoneen etusivun ja tallentaa sen tiedoston `google.html` sisälle. Huomaa, että olemme käyttäneet `Data.ByteString`-kirjastoa, joka mahdollistaa binaaridatan käsittelyn, ja `Network.Wreq`-kirjastoa, joka tarjoaa yksinkertaisen tavan tehdä HTTP-pyyntöjä. Tämän lisäksi olemme käyttäneet `let`-lauseketta ja `do`-syntaksia tekemään latausoperaation ja tiedoston tallentamisen.

## Syvällinen tarkastelu

Jos olet kiinnostunut syvemmin siitä, miten web-sivuja ladataan Haskellilla, suosittelemme tutustumaan `wget`-kirjaston lähdekoodiin ja tutkimaan, miten se käsittelee HTTP-pyyntöjä. Voit myös käyttää eri kirjastoja, kuten `http-conduit` tai `curl`, joilla on erilaisia ominaisuuksia ja toimintatapoja.

## Katso myös

- [wget-kirjasto Hackageissa](http://hackage.haskell.org/package/wget)
- [Tulosta HTTP-kuorma Haskellissa](https://stackoverflow.com/questions/3854273/print-the-http-load-using-haskell)
- [Haskellin perusopas](https://wiki.haskell.org/Learn_Haskell)