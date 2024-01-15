---
title:                "Alimerkintöjen eristäminen"
html_title:           "Elm: Alimerkintöjen eristäminen"
simple_title:         "Alimerkintöjen eristäminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit erottaa osa merkkijonosta? Yksinkertaisesti sanottuna, koska joskus sinun täytyy työskennellä vain osan siitä. Ehkä haluat tulostaa vain päivämäärän tai vain osaa sähköpostiosoitteesta. Tämä on, missä substrings (osa merkkijonoista) tulevat peliin.

## Miten

```Elm
myString = "Tämä on esimerkki!"
substr = String.slice 4 15 myString
```

Tässä esimerkissä käytimme `String.slice` -funktiota erottamaan myString-muuttujasta osa merkkijonoa. Ensimmäinen parametri on aloitusindeksi ja toinen on lopetusindeksi. Tuloksena on uusi merkkijono, joka alkaa "on" ja päättyy "imerkki". Joten tulos on "on esimerkki".

```Elm
myString = "0912345678"
substr = String.left 3 myString
```

Toisessa esimerkissä käytimme `String.left` -funktiota erottamaan myString-muuttujasta vain kolme ensimmäistä numeroa. Joten tulos on "091".

Voit myös käyttää `String.drop` -funktiota poistamaan tietyn määrän merkkejä merkkijonon alusta. Esimerkiksi `String.drop 2 myString` poistaisi ensimmäiset kaksi merkkiä, joten tuloksena olisi "mä on esimerkki!".

## Syvällinen sukellus

Erikoistapauksissa voit tarvita enemmän kuin vain `String.slice`, `String.left` ja `String.drop`. Tällöin kannattaa tutkia `String.at`, `String.substring` ja `String.split` -funktioita. `String.at` antaa sinulle yhden merkin tietystä indeksistä, `String.substring` ei pelkästään leikkaa merkkijonoa tietystä kohdasta, mutta myös sallii negatiiviset indeksit ja `String.split` antaa sinulle listan merkkijonon osista tietyn erotinmerkin perusteella.

## Katso myös

- [Elm String -dokumentaatio](https://package.elm-lang.org/packages/elm-community/string-extra/latest/)
- [Elm String -esimerkkejä](https://elm-lang.org/examples)