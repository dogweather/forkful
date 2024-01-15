---
title:                "Merkkijonon pääkirjainmuunnos"
html_title:           "Elm: Merkkijonon pääkirjainmuunnos"
simple_title:         "Merkkijonon pääkirjainmuunnos"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluat muuntaa tekstin isoiksi kirjaimiksi? Se voi olla hyödyllistä esimerkiksi, kun haluat korostaa tärkeää sanaa tai otsikkoa.

## Kuinka

Yksinkertaisin tapa muuttaa merkkijono isoiksi kirjaimiksi on käyttää `toUpper` -funktiota.

```Elm
import String

capitalizedWord = String.toUpper "tämä on isoiksi kirjaimiksi"
-- "TÄMÄ ON ISOIKSI KIRJAIMIKSI"
```

Voit myös muuttaa vain tietyn alueen merkkijonosta isoiksi kirjaimiksi käyttämällä `toUpper` yhdessä `slice` -funktion kanssa.

```Elm
import String

partialCapitalizedWord = String.slice 0 5 (String.toUpper "tämä on isoiksi kirjaimiksi")
-- "TÄMÄ "
```

## Syvällinen sukellus

Elm-kielessä merkkijonot ovat muuttumattomia, mikä tarkoittaa, että merkkijonon muuttaminen vaatii uuden merkkijonon luomista ja alkuperäisen merkkijonon hylkäämistä. Tästä syystä on hyvä välttää merkkijonojen jatkuvaan muokkaamiseen perustuvaa ohjelmointityyliä, sillä se voi aiheuttaa turhaa suorituskyvyn laskua.

## Katso myös

- [Ohjelmoinnin perusteet: Merkkijonon muuttaminen](https://guide.elm-lang.org/language_basics/strings.html)
- [Elm-paketin dokumentaatio: String](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Muut tapoja muuntaa merkkijonoja isoiksi kirjaimiksi](https://stackoverflow.com/questions/34995233/how-to-make-a-word-uppercase-in-elm)