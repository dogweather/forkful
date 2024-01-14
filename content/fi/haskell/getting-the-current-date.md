---
title:                "Haskell: Hanki nykyinen päivämäärä"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

Haskell ohjelmoinnissa on paljon hyödyllisiä ominaisuuksia, joita voi käyttää päivittäisessä ohjelmoinnissa. Yksi tärkeimmistä on tietyn päivämäärän saaminen. Tässä blogikirjoituksessa opit, kuinka saat tämän päivän päivämäärän käyttämällä Haskellia.

## Miksi?

Päivämäärän saaminen on tärkeää monessa ohjelmassa. Se voi auttaa sinua luomaan tehokkaampia ja paremmin jäsenneltyjä ohjelmia. Lisäksi päivämäärä voi auttaa seurata aikaa ja päivämäärästä riippuvia tapahtumia, kuten laskutuksia. 

## Kuinka?

Aloitetaan luomalla yksinkertainen ohjelma, joka käyttää Haskellin `getCurrentTime` -funktiota saadaksesi tämän päivän päivämäärän. 

```Haskell
-- Tuodaan tarvittava kirjasto
import Data.Time.Clock (getCurrentTime)

-- Luodaan funktio, joka palauttaa tämän päivän päivämäärän
getToday = do
  time <- getCurrentTime -- Käytetään getCurrentTime -funktiota
  return (show time) -- Palautetaan päivämäärä tekstinä

main = do
  today <- getToday -- Kutsutaan getToday -funktiota ja tallennetaan palautusmuuttujaan
  print today -- Tulostetaan tulos
```

Tämä ohjelma tulostaa tämän päivän päivämäärän seuraavassa muodossa:

`2021-04-07 11:46:29.890116 UTC`

Voit myös muuttaa päivämäärän muodon käyttämällä erilaisia funktioita, kuten `formatTime` tai `toGregorian`. Kokeile rohkeasti erilaisia vaihtoehtoja ja löydä itsellesi sopivin ratkaisu.

## Syväsukellus

Haskellin päivämäärä- ja aikafunktiot perustuvat `Data.Time` -kirjastoon. Tämä kirjasto tarjoaa erilaisia työkaluja, joilla voit käsitellä päivämääriä, aikoja ja niiden eroja. Voit myös käydä läpi `Data.Time` -kirjaston dokumentaatiota löytääksesi lisää hyödyllisiä funktioita ja esimerkkejä.

On myös tärkeää huomata, että päivämäärät ja ajat voivat olla haastavia käsitellä. Kannattaa varmistaa, että ymmärrät hyvin miten käytät funktioita ja että otat huomioon erilaiset aikavyöhykkeet ja päivämäärämuodot.

## Katso myös

- Haskellin virallinen dokumentaatio `Data.Time` -kirjastosta: https://hackage.haskell.org/package/time-1.9/docs/Data-Time.html
- Hyödyllinen opetusohjelma päivämäärän ja ajan käsittelyyn Haskellissa: https://mmhaskell.com/blog/2017/5/29/time-and-space-part-1
- Tietoa päivämäärien säilyttämisestä tietokannoissa Haskellilla: https://www.robhu.org/2018/05/25/storing-dates-in-postgres-using-haskell/