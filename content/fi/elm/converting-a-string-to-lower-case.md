---
title:                "Merkkijonon muuttaminen pienikirjaimiseksi"
html_title:           "Elm: Merkkijonon muuttaminen pienikirjaimiseksi"
simple_title:         "Merkkijonon muuttaminen pienikirjaimiseksi"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?
Miksi ohjelmoijat muuttavat merkkijonon pieniksi kirjaimiksi? Tämä tehdään yleensä datasta tehdyssä analyysissä, jotta voimme käsitellä dataa yhtenäisesti ja helpottaa esimerkiksi filtteröintiä ja lajittelua.

## Miten:
```Elm
toLower "OHJELMISTO" -- tulos "ohjelmisto"
toLower "HeLLo WOrLD" -- tulos "hello world"
```

## Syvällistä tietoa:
Tämä toiminto oli alunperin kehitetty toisessa ohjelmointikielessä, nimeltään Haskell. Se on myös yksi usein käytetyistä esimerkeistä, kun esitellään funktionaalista ohjelmointia. Vaihtoehtoisesti, voit myös käyttää funktiota `String.toLowercase`, joka tekee saman asian. Tämä toiminto muuttaa kaikki merkit pieniksi kirjaimiksi, mutta säilyttää muut erikoismerkit sellaisinaan.

## Katso myös:
Lisätietoa merkkijonon muuttamisesta pieniksi kirjaimiksi löydät [Elm-lähteistä](https://guide.elm-lang.org/effects/string.html#to-lower) sekä [Haskell lähteistä](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Char.html#g:4).