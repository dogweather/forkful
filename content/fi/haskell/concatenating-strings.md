---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Haskell: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

# Miksi ja miten yhdistää merkkijonoja Haskellilla

## Mikä ja miksi?
Concatenating strings eli merkkijonojen yhdistäminen on yleinen ohjelmointitekniikka, jossa kaksi tai useampi merkkijono yhdistetään yhdeksi merkkijonoksi. Tätä tarvitaan usein esimerkiksi tulostettaessa tekstiä eri muuttujien arvojen kanssa tai luotaessa uusia merkkijonoja. Haskellia käyttävät ohjelmoijat käyttävät tätä tekniikkaa helpottamaan työtään ja saadakseen halutunlaisia tuloksia aikaan.

## Miten?
Haskellissa merkkijonojen yhdistäminen tapahtuu käyttäen operaattoria ```++```. Se ottaa kaksi merkkijonoa ja yhdistää ne yhdeksi uudeksi merkkijonoksi. Esimerkiksi:

```Haskell
"Graafinen" ++ "käyttöliittymä"
```

Palauttaa tuloksen: *Graafinen käyttöliittymä*. Merkkijonoja voidaan myös yhdistää muuttujien kanssa, esimerkiksi:

```Haskell
"Koodaamme" ++ " " ++ programmingLanguage
```

Missä ```programmingLanguage``` on esim. muuttuja, joka sisältää arvon "Haskell". Tämä palauttaa tuloksen: *Koodaamme Haskellia*.

Merkkijonojen yhdistäminen voidaan tehdä myös käyttämällä ```concat```-funktiota, joka ottaa listan merkkijonoja ja yhdistää ne yhdeksi merkkijonoksi. Esimerkiksi:

```Haskell
concat ["1","2","3"]
```

Palauttaa tuloksen: *123*.

## Syväsukellus
Merkkijonojen yhdistämistä on käytetty ohjelmoinnissa jo pitkään. Aiemmin se oli usein hidas toimenpide, mutta Haskellissa ```++```-operaattorin ja ```concat```-funktion käyttö tekee siitä tehokkaamman ja nopeamman. Kannattaa myös tutustua muihin tapoihin käsitellä merkkijonoja Haskellissa, kuten ```intercalate```-funktioon, joka yhdistää merkkijonolistassa olevat merkkijonot halutun erotinmerkin avulla.

## Katso myös
- [Haskellin virallinen dokumentaatio](https://www.haskell.org/documentation/)
- [Tutorialspointin opas Haskellin merkkijonojen yhdistämiseen](https://www.tutorialspoint.com/haskell/haskell_strings.htm)
- [Real World Haskell -kirjan luku merkkijonomenetelmistä ja -analyysistä](http://book.realworldhaskell.org/read/efficient-file-processing-regular-expressions-and-file-name-matching.html)