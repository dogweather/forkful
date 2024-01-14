---
title:                "Haskell: Alaotsikoiden erottaminen"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluatte etsiä alimerkkijonoja Haskellissa? On monia tilanteita, joissa on tarpeen hakea tietoa merkkijonoista, kuten analysoitaessa käyttäjän syöttämiä tietoja tai prosessoidessa tekstipohjaisia tiedostoja.

## Näin teet sen

Etsitään ensin yksittäinen alimerkkijono merkkijonosta käyttäen `take` ja `drop` -funktioita.

```Haskell
let merkkijono = "Hei maailma!"
let alimerkkijono = take 3 merkkijono  -- alimerkkijono = "Hei"
let uusiMerkkijono = drop 5 merkkijono -- uusiMerkkijono = "maailma!"
```

Voit myös hakea useita alimerkkijonoja samalla kertaa käyttäen `substring` -funktiota.

```Haskell
let merkkijono = "Tämä on esimerkki."
let alimerkkijonot = substring 5 10 merkkijono -- alimerkkijonot = "on esim"
```

## Syvemmälle

Haskellissa alimerkkijonojen hakeminen perustuu `take` ja `drop` -funktioihin, jotka ottavat parametreinaan merkkijonon ja määritellyn alueen. `substring` -funktio puolestaan yhdistää nämä kaksi funktiota ja mahdollistaa useiden alimerkkijonojen hakuun.

On myös muita tapoja hakea alimerkkijonoja, kuten käyttämällä säännöllisiä lausekkeita `regex` kirjaston avulla.

## Katso myös

- [Haskellin `String` dokumentaatio](https://www.haskell.org/onlinereport/standard-prelude.html#t:String)
- [Haskellin `substring` dokumentaatio](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-List.html#v:substring)
- [Regex kirjasto Haskellille](https://hackage.haskell.org/package/regex)