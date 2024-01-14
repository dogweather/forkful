---
title:    "Haskell: Uuden projektin aloittaminen"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Miksi

Monet ovat kiinnostuneita aloittamaan uuden Haskell-projektin, mutta eivät tiedä mistä aloittaa. Tässä blogikirjoituksessa käymme läpi, miksi kannattaa lähteä uuteen projektiin ja mitä siihen tarvitaan.

## Miten aloittaa

Ensimmäinen askel uuden Haskell-projektin aloittamisessa on varmistaa, että tarvittavat työkalut ovat asennettuna. Tämän jälkeen voidaan luoda uusi projektikansio ja navigoida sinne terminaalissa. Alla on esimerkki uuden projektin luomisesta ja sen tuottamasta tiedostorakenteesta:

```Haskell
stack new projektin_nimi
```

Tämän jälkeen voidaan vaihtaa luotuun kansioon ja avata se halutussa kehitysympäristössä, kuten esimerkiksi Visual Studio Codessa. Seuraavaksi luodaan ensimmäinen moduuli, jossa määritellään muutama funktio:

```Haskell
module Main where

-- Laskee kahden luvun summan
summa :: Int -> Int -> Int
summa x y = x + y

-- Kerrotaan luku kolmella
kolminkertainen :: Int -> Int
kolminkertainen x = x * 3

-- Tulostetaan laskujen tulos
main :: IO ()
main = print $ summa 5 (kolminkertainen 4)
```

Projektin voi nyt suorittaa ajamalla seuraavan komennon:

```Haskell
stack run
```

Tulosteena saadaan laskujen tulos, joka tässä tapauksessa on 17. Tämän esimerkin avulla päästään alkuun uuden projektin luomisessa ja koodin kirjoittamisessa.

## Syvempi sukellus

Uuden Haskell-projektin aloittaminen voi aluksi tuntua haastavalta, mutta kokonaisuudessaan se on hyvin samanlaista kuin muissa kielissä. Tärkeintä on aloittaa pienestä ja lisätä ominaisuuksia ja moduuleja vähitellen.

Ensimmäinen haaste voi olla ohjelmointiympäristön valinta, sillä mahdollisuuksia on useita. On tärkeää löytää itselleen sopiva ympäristö, jossa on helppo työskennellä ja joka tukee Haskellin ominaisuuksia. Toinen haaste voi olla koodin ymmärtäminen, sillä Haskellin syntaksi eroaa monista muista kielistä. Tässä auttaa harjoittelu ja lukeminen sekä esimerkkikoodien että dokumentaation avulla.

Uuden projektin aloittamisessa on myös tärkeää miettiä, millaiseen tarkoitukseen sitä tarvitaan ja millaisia ominaisuuksia siihen halutaan lisätä. Esimerkiksi web-sovellusten kehittämiseen on olemassa käteviä Haskell-kirjastoja, jotka helpottavat työskentelyä ja lisäävät valmiita toiminnallisuuksia.

## Katso myös

- [Haskell-aloittelijan opas](https://wiki.haskell.org/Haskell_beginners_steps)
- [Haskellin dokumentaatio](https://www.haskell.org/documentation/)
- [Haskell-web-kehitys kirjastoilla](https://haskell-lang.org/web)
- [Haskell Stack -työkalun käyttöohjeet](https://docs.haskellstack.org/en/stable/GUIDE/)
- [Visual Studio Coden Haskell-laajennus](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)