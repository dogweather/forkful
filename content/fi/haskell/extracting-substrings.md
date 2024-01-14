---
title:    "Haskell: Osien erottelu"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Olet ehkä kuullut puhuttavan "merkkijonojen erottamisesta" tai "merkkijonojen jakamisesta" ohjelmoinnissa. Tiedät kuitenkin, että jos olet kokenut Haskell-ohjelmoija, tämä ei välttämättä kuulosta niin yksinkertaiselta. Miksi sitten haluaisit erottaa merkkijonoja osiin? Onko se todella niin tärkeä taito?

Haskellissa merkkijonojen erottaminen on välttämätöntä monissa tilanteissa. Se voi auttaa sinua löytämään tietyn osan merkkijonosta, suorittamaan tietyn toiminnon tietyllä osalla tai tarkistamaan, onko merkkijonossa tietty merkkijonoyhdistelmä. Joten haluatko tietää, miten tämä tehdään? Lue eteenpäin!

## Kuinka

Aloitetaan yksinkertaisesta tilanteesta. Kuvitellaan, että meillä on merkkijono "Tämä on esimerkki merkkijonosta". Haluamme erottaa tämän merkkijonon kolmeen osaan: "Tämä on", "esimerkki" ja "merkkijonosta". Tämä voidaan tehdä hyödyntämällä haskellia sisäänrakennettua "words" -funktiota. Esimerkiksi seuraavassa koodissa erotamme merkkijonon ja tulostamme jokaisen sanan omalle rivilleen:

```Haskell
let str = "Tämä on esimerkki merkkijonosta"
let wordList = words str
print wordList
```

Tämä tuottaa seuraavan tuloksen:

```
["Tämä", "on", "esimerkki", "merkkijonosta"]
```

Mutta mitä jos haluamme erottaa merkkijonon tietyn merkkijonon perusteella? Esimerkiksi haluamme erottaa merkkijonosta vain "esimerkki" ja "merkkijonosta". Tämä voidaan tehdä käyttämällä "split"-funktiota kirjastosta "Data.List.Split". Seuraava esimerkki näyttää kuinka tämä voidaan tehdä:

```Haskell
import Data.List.Split

let str = "Tämä on esimerkki merkkijonosta"
let subStrs = splitOn " " str
print subStrs
```

Tämä antaa meille tuloksen:

```
["Tämä", "on", "esimerkki", "merkkijonosta"]
```

Kuten näet, annettu merkkijono on jaettu välilyöntien perusteella osiin.

## Pohdintaa

Merkkijonojen erottamiseen on monia erilaisia tapoja Haskellissa. Edellä mainittujen esimerkkien lisäksi voit myös käyttää "break"-funktiota, joka jakaa merkkijonon tietyn ehdon (esimerkiksi välimerkin) perusteella. Voit myös käyttää haskellin syntaksin ominaisuuksia, kuten listojen hajauttamista, helpottamaan merkkijonojen erottamista.

Muista myös, että merkkijonot ovat muuttumattomia haskellissa, joten merkkijonojen erottamisen sijasta aina luodaan uusi merkkijono, joka sisältää vain halutun osan alkuperäisestä merkkijonosta.

Tarpeen mukaan voit myös itse luoda omia funktioita merkkijonojen erottamiseen. Tämä voi auttaa sinua parantamaan ymmärrystäsi haskellin syntaksista ja toiminnasta.

## Katso myös

- [Haskellin "words"-funktion dokumentaatio] (https://hackage.haskell.org/package/base/docs/P