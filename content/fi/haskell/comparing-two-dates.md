---
title:    "Haskell: Kahden päivämäärän vertailu"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Miksi vertailla kahden päivämäärän välillä?

Päivämäärien vertaileminen on tärkeä osa monia ohjelmointitehtäviä, esimerkiksi tapahtumien järjestämistä tai aikapohjaista laskentaa. Tällaisissa tilanteissa on tärkeää pystyä vertailemaan kahta eri päivämäärää ja selvittämään, kumpi niistä on aikaisempi tai myöhempi. Onneksi Haskell tarjoaa helpon ja tehokkaan tavan käsitellä päivämääriä ja tehdä niiden vertailua.

## Miten vertailla kahden päivämäärän välillä Haskellilla?

Jos haluat vertailla kahden päivämäärän välillä Haskellissa, ensimmäinen askel on tuoda käyttöön `Data.Time` -moduuli. Tämä moduuli tarjoaa paljon hyödyllisiä toimintoja päivämäärien käsittelyyn ja vertailuun.

Tässä on esimerkki kahden päivämäärän vertailusta:

```Haskell
import Data.Time

-- Luodaan kaksi päivämäärää "2020-01-20" ja "2020-02-10"
date1 = fromGregorian 2020 1 20
date2 = fromGregorian 2020 2 10

-- Vertaillaan päivämääriä käyttämällä toimintoa "compare"
date1 `compare` date2
-- Output: LT (eli "Less Than")

-- Voit myös käyttää muita vertailutoimintoja, kuten "==" tai ">"
date1 == date2
-- Output: False
```

Kuten esimerkistä nähdään, päivämäärien vertailu tapahtuu käyttämällä `compare` -funktiota, joka palauttaa `Ordering` -tyypin tuloksen. Tämä tarkoittaa, että päivämäärien vertailu on toteutettu verrattavissa olevien arvojen vertailun periaatteella.

## Syvemmälle päivämäärien vertailuun

Päivämäärien vertailu perustuu niiden lukuarvoihin, ja sitä voi tehdä myös yksityiskohtaisemmin käyttämällä `Day` -tyypin sisäisiä funktioita. Esimerkiksi päivämäärän päivä-, kuukausi- ja vuosiluvut voi hakea ja vertailla erikseen.

Lisäksi päivämäärät voi muuntaa eri muotoihin, kuten merkkijonoiksi tai Unix-timestampeiksi. Tämä tarjoaa lisää mahdollisuuksia päivämäärien käsittelyyn ja vertailuun erilaisissa ohjelmointitehtävissä.

## Katso myös

- [Data.Time -moduulin dokumentaatio](http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [Haskellin vertailuoperaattorit](https://www.tutorialspoint.com/haskell/haskell_operators.htm)
- [Ohio State Universityn opetusvideo päivämäärien vertailusta Haskellissa](https://www.youtube.com/watch?v=v0EzLE19AFM)