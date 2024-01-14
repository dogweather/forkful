---
title:                "Elm: Merkkijonon muuttaminen isoin kirjaimin"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisimme muuttaa merkkijonon alkukirjaimen isoksi Elm-ohjelmointikielessä? Yhtenä syynä voisi olla visuaalinen halu, esimerkiksi tekstin esittäminen otsikkona tai korostettuna. Toisena syynä taas voi olla tietojen standardointi, joka voi vaatia tiettyä muotoa merkkijonolle.

## Kuinka tehdä

Elm-kielen `String`-moduuli tarjoaa `toUpper`-funktion, joka muuttaa merkkijonon kaikki kirjaimet isommiksi. Voit käyttää sitä seuraavalla tavalla:

```Elm
import String

teksti = "tämä on esimerkki"
muutettuTeksti = String.toUpper teksti

-- Tulos: "TÄMÄ ON ESIMERKKI"
```

Olemme luoneet uuden muuttujan `muutettuTeksti`, joka sisältää samaa merkkijonoa, mutta kaikki kirjaimet ovat nyt isommat. Voit myös käyttää `String.toUpper`-funktion sijasta `String.toTitle`, joka muuttaa ainoastaan ensimmäisen kirjaimen isoksi.

```Elm
esimerkki = "esimerkki teksti"
muutettuEsimerkki = String.toTitle esimerkki

-- Tulos: "Esimerkki teksti"
```

## Syvällisempi tarkastelu

Merkkijonon muuttaminen isoksi on monimutkainen prosessi, joka vaatii tarkkaa käsittelyä. Elm-kielen `String`-moduuli tarjoaa kuitenkin useita käteviä toimintoja, jotka helpottavat tätä prosessia. Voit esimerkiksi käyttää `String.length`-funktiota saadaksesi selville merkkijonon pituuden. Lisäksi voit käyttää `String.toLower`-funktiota muuttaaksesi kaikki kirjaimet pieniksi.

```Elm
merkkijono = "Esimerkki Teksti"
pituus = String.length merkkijono
pieniMerkkijono = String.toLower merkkijono

-- Tulos: 16, "esimerkki teksti"
```

## Katso myös

- [Elm-kielen String-moduuli](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Merkkijonon manipulointi Elm-kielen avulla](https://dev.to/mattjamesbriggs/manipulating-strings-in-elm-2cj5)
- [Elm-ohjelmointikielen virallinen sivusto](https://elm-lang.org/)