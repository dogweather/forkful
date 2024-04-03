---
date: 2024-01-20 17:47:31.205422-07:00
description: "How to: (Kuinka tehd\xE4:) Elm tarjoaa `String.length` funktion, joka\
  \ ottaa merkkijonon (`String`) ja palauttaa pituuden (`Int`)."
lastmod: '2024-03-13T22:44:56.479562-06:00'
model: gpt-4-1106-preview
summary: Elm tarjoaa `String.length` funktion, joka ottaa merkkijonon (`String`) ja
  palauttaa pituuden (`Int`).
title: "Merkkijonon pituuden selvitt\xE4minen"
weight: 7
---

## How to: (Kuinka tehdä:)
Elm tarjoaa `String.length` funktion, joka ottaa merkkijonon (`String`) ja palauttaa pituuden (`Int`):

```Elm
import Html exposing (text)

main =
    text (String.fromInt (String.length "Hei Suomi!"))

-- Tulos on näyttöruudulla: 10
```

Tässä tapauksessa me muunnamme pituuden numerosta (`Int`) merkkijonoksi funktion `String.fromInt` avulla, jotta voimme näyttää sen nettisivulla.

## Deep Dive (Syvä sukellus)
Stringien pituuden laskeminen on perustavanlaatuinen ohjelmoinnin konsepti, mikä on ollut mukana ohjelmointikielissä pitemmän aikaa. Elm, kuten moni muukin funktionaalinen kieli, toimii immutaabelien merkkijonojen kanssa. Tämä tarkoittaa, että kun lasket stringin pituutta, et muuta alkuperäistä merkkijonoa.

Vaihtoehtoisia tapoja selvittää stringin pituutta Elmissä ei periaatteessa ole, koska Elm pyrkii yksinkertaiseen ja selkeään API:in. Tämä auttaa ohjelmoijia välttämään virheitä ja sekaannusta.

Stringin pituuden laskenta on suoraviivaista, mutta yksi yksityiskohta on tärkeä: Elm käsittelee UniCode-merkkejä oikeaoppisesti. Tämä tarkoittaa, että jos stringissäsi on esimerkiksi emoji, se lasketaan yhdeksi merkiksi sen teknisestä toteutuksesta huolimatta.

## See Also (Katso myös)
- Elm `String` moduulin dokumentaatio: [Elm String docs](http://package.elm-lang.org/packages/elm/core/latest/String)
- Unicode standardi ja merkkien käsittely: [Unicode Consortium](https://home.unicode.org/)
- Elm-yhteisön keskustelut ja ohjeet merkkijonojen käsittelystä: [Elm Discourse](https://discourse.elm-lang.org/)
