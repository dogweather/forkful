---
title:    "Haskell: Stringien yhdistäminen"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointikielissä on tarpeen yhdistää kaksi tai useampia merkkijonoja yhdeksi kokonaisuudeksi. Tämä voi olla hyödyllistä esimerkiksi tekstin muotoilemisessa tai tietokantaan tallennettaessa. Onneksi Haskellissa stringien yhdistäminen on helppoa ja tehokasta!

## Kuinka tehdä

Haskellissa stringien yhdistäminen tapahtuu `++` operaattorilla tai `concat` funktion avulla. Katsotaanpa pari esimerkkiä:

```
-- Yhdistetään kaksi merkkijonoa
"Hello " ++ "world!" 
-- Tulostaa: "Hello world!"

-- Yhdistetään lista merkkijonoja
concat ["1", "2", "3"]
-- Tulostaa: "123"
```

Haskellissa on myös mahdollista käyttää `foldl (+) ""` funktiota, jolla voi yhdistää listan merkkijonoja yhdeksi kokonaisuudeksi. Tämä voi olla hyödyllistä silloin, kun tarvitsemme yhdistää suuren määrän merkkijonoja.

## Syvällisempi sukellus

Stringien yhdistäminen Haskellissa on tehokasta, sillä kieli käyttää Lazy Evaluation -toimintoa. Tämä tarkoittaa sitä, että merkkijonot eivät ole muistissa ennen kuin niitä tarvitaan, mikä säästää muistia ja prosessointiaikaa. Toisaalta tämä voi aiheuttaa ongelmia, mikäli haluamme yhdistää merkkijonoja järjestyksessä ja saada oikean tuloksen. Tällaisessa tapauksessa on parempi käyttää `foldl (+) ""` funktiota, joka käsittelee stringit järjestyksessä.

## Katso myös

- [Haskellin dokumentaatio stringien yhdistämisestä](https://www.haskell.org/tutorial/string.html#concatenation)
- [Haskellin virallinen verkkosivusto](https://www.haskell.org/)
- [Haskell-opetusohjelma suomeksi](https://wiki.haskell.org/Suomi/Haskell-opetusohjelma)