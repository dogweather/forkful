---
title:    "Elm: Alimerkkijonojen hakeminen"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi käyttää ohjelmoinnissa erottamisen substansseja? Substringien erottaminen on tärkeä työkalu, joka auttaa manipuloimaan ja käsittelyssä tekstiä. Tämä on erityisen hyödyllistä, kun käsitellään suuria määriä tietoa ja halutaan poimia vain tiettyjä osia siitä.

## Kuinka

Erottaminen substringeja Elm-kielellä on helppoa! Käytä vain funktiota `String.slice` ja anna sille kolme parametria: merkkijono, alkukohta ja lopetuskohta. Tämä palauttaa alkuperäisestä merkkijonosta osan haluttujen aloitus- ja lopetuspisteiden väliltä.

```Elm
let merkkijono = "Tämä on esimerkki"
let osa = String.slice merkkijono 4 9

--Tämä palauttaa "on es" 

```

Voit myös käyttää `String.substitute` funktiota, joka korvaa halutut merkit tai merkkijonot toisilla.

```Elm
let merkkijono = "Tämä on esimerkki"
let uusiMerkkijono = String.substitute "-" " " merkkijono

--Tämä palauttaa "Tämä-on-esimerkki"
``` 

Voit myös käyttää `String.split` funktiota, joka jakaa merkkijonon annetun merkin tai merkkijonon kohdalta ja palauttaa listan osista.

```Elm
let merkkijono = "Omena, Banaani, Appelsiini"
let osat = String.split ", " merkkijono

--Tämä palauttaa ["Omena", "Banaani", "Appelsiini"]
```

## Syvemmälle

Erottaminen substringeja on tärkeä osa merkkijonojen käsittelyssä, mutta se vaatii myös tarkkuutta. Esimerkiksi, jos aloitus- tai loppukohta eivät ole merkkijonon sisällä, tulee viestinä `String.slice` yrittäessä suorittaa operaatiota.

Lisäksi, kannattaa muistaa että merkkijonat ovat muuttumattomia Elm-kielessä, eli `String.slice` palauttaa uuden merkkijonon sen sijaan että muokkaisi alkuperäistä.

## Katso myös

- [Elm ohjelmointikieleen virallisilla sivuilla](https://elm-lang.org/)
- [String-moduulin dokumentointi Elmissä](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm-kielen dokumentointi](https://elm-lang.org/docs)