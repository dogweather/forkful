---
title:    "Elm: Päivämäärän muuttaminen merkkijonoksi"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Miksi: Päivämäärän muuttaminen merkkijonoksi
Monissa ohjelmointiprojekteissa on tarve käsitellä päivämääriä ja näyttää ne käyttäjille ymmärrettävässä muodossa. Tämä onnistuu helposti muuttamalla päivämäärä merkkijonoksi, jota voi sitten näyttää esimerkiksi käyttöliittymässä. Elm tarjoaa kätevän tavan muuttaa päivämäärä merkkijonoksi, josta kerromme seuraavassa osiossa.

## Kuinka tehdä se
Merkkijonoksi muuttamisessa käytetään Elm:n Date-moduulia ja sen toText-funktiota. Date-moduulissa on valmiina funktioita päivämäärän käsittelemiseen ja se tarjoaa myös hyödyllisiä apufunktioita muutoksiin. Alla on esimerkki koodista, jossa muutetaan tänään oleva päivämäärä merkkijonoksi muotoon "päivä.kuukausi.vuosi".

```Elm
import Date

Date.toText (Date.today) Date.Day "fi-FI"
```

Esimerkissä käytetty toText-funktio saa parametreikseen päivämäärän, muutettavan muodon sekä halutun kielen. Koodin tulostettaessa saamme merkkijonon "31.03.2021".

## Syvempää tietoa
Date-moduulin toText-funktio tarjoaa myös muita vaihtoehtoja päivämäärän muotoilemiseen, kuten eri kelloajan esittämisen ja kuukauden lyhyemmän nimen käyttämisen. Lisätietoa ja esimerkkejä löydät Elm:n virallisesta dokumentaatiosta.

## Katso myös
- [Elm:n virallinen dokumentaatio] (https://guide.elm-lang.org)
- [Elm Date-moduuli] (https://package.elm-lang.org/packages/elm/time/latest/Date)