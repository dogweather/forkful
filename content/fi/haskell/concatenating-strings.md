---
title:                "Haskell: Merkkijonojen yhdistäminen"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Haskell-ohjelmoinnin yksi tärkeimmistä osa-alueista on merkkijonojen liittäminen. Tämä tarkoittaa, että voit yhdistää useita merkkijonoja yhdeksi kokonaisuudeksi. Tämä on erittäin hyödyllistä, kun haluat esimerkiksi näyttää käyttäjälle yhdistetyn tekstin tai tallentaa tiedostoja nimillä, jotka koostuvat useista merkkijonoista.

## Miten tehdä

Merkkijonojen liittämiseen Haskellissa on muutama eri tapa. Yksi vaihtoehto on käyttää `++`-operaattoria, joka yhdistää kaksi merkkijonoa. Esimerkiksi, jos haluat yhdistää merkkijonot "Hei" ja "maailma", voit tehdä sen seuraavasti:

```Haskell
"Hei" ++ "maailma"
```

Tämän koodin tulosteena on "Heimaailma". Huomaa, että välissä ei ole välilyöntiä, joten on hyvä muistaa lisätä se tarvittaessa.

Toinen tapa liittää merkkijonoja on käyttää `concat`-funktiota. Tämä funktio voi ottaa vastaan useita merkkijonoja ja yhdistää ne yhdeksi. Esimerkiksi:

```Haskell
concat ["Hei", " ", "maailma"]
```

Tämä tuottaa saman tuloksen kuin edellinen esimerkki. Voit myös käyttää `concat`-funktiota listalle, joka sisältää merkkijonoja, kuten tässä tapauksessa.

## Syvempi sukellus

Haskellissa merkkijonojen liittäminen on tehokasta, sillä ne ovat käytännössä vain lista merkkejä. Tämä tarkoittaa, että merkkijonoja voi käsitellä samoin kuin listoja ja käyttää monia samoja funktioita ja operaattoreita. Esimerkiksi `++`-operaattori toimii myös listoille ja `concat`-funktio voi yhdistää myös ei-merkkijonoista koostuvia listoja.

Merkkijonojen liittämiseen liittyy myös joitakin käytännön vinkkejä. Esimerkiksi, jos sinulla on suuri määrä merkkijonoja, jotka haluat yhdistää, olisi parempi käyttää `Data.Text`-moduulin `intercalate`-funktiota, joka on optimoitu suuremmille datamäärille.

## Katso myös

- [Haskellin merkkijonojen liittäminen Dokumentaatio](https://www.haskell.org/onlinereport/standard-prelude.html#cat)
- [Data.Text-moduulin dokumentaatio](https://hackage.haskell.org/package/text/docs/Data-Text.html)
- [Haskellin perusteet - oppimateriaali merkkijonot osio](https://github.com/mooc-fi/haskell-perusteet/tree/master/tehtavat/Merkkijonot)