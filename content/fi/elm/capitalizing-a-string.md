---
title:    "Elm: Merkkijonon ensimmäisen kirjaimen suurennus"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Joskus tarvitsemme päästä käsiksi käyttäjän syöttämiin tietoihin ja muokata niitä ohjelmassamme. Tämä koskee myös merkkijonoja, joiden suuri- tai pienikirjaimisuus voi olla tärkeä merkityksen kannalta. Tässä blogikirjoituksessa opimme miten voi muuttaa merkkijonon kirjainten kokoa Elm-ohjelmointikielessä.

## Miten

Usein haluamme muuttaa merkkijonon ensimmäisen kirjaimen suureksi kirjaimeksi ja muokata sen sisältöä. Alla on esimerkki, jossa muutamme "hei" merkkijonon "Hei" merkkijonoksi käyttämällä Elm:n `String`-moduulia:

```
Elm.String.capitalize "hei" -- palauttaa "Hei"
```

Voit myös käyttää `toUpper` ja `toLower` funktioita muuttaa koko merkkijono suuriksi tai pieniksi kirjaimiksi:

```
import String exposing (toUpper, toLower)

toUpper "moikka" -- palauttaa "MOIKKA"
toLower "MOIKKA" -- palauttaa "moikka"
```

## Syventyminen

Elm-ohjelmointikielessä on myös muita tapoja muuttaa merkkijonon kirjainten kokoa. Voit esimerkiksi käyttää `map` funktiota ja `String.toUpper` tai `String.toLower` funktioita muuttaaksesi kaikki merkkijonon kirjaimet haluamaasi kokoon. Alla on esimerkki, jossa muutamme "moikka" merkkijonon "MOIKKA" merkkijonoksi käyttämällä `map` funktiota:

```
import String exposing (map, toUpper, toLower)

map toUpper "moikka" -- palauttaa "MOIKKA"
```

## Katso myös

- [Elm:n String-moduuli](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm:n map funktio](https://package.elm-lang.org/packages/elm/core/latest/List#fromList)
- [Elm:n ohjelmointikieleen tutustuminen](https://guide.elm-lang.org/index.html)