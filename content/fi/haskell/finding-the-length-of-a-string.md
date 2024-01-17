---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "Haskell: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mikä & miksi?
Merkkijonon pituuden selvittäminen on tärkeä ohjelmointikäytäntö, jolla mitataan merkkijonon sisältämien merkkien määrää. Tämä on hyödyllistä esimerkiksi kun tarkastellaan syötteitä tai tulostetaan tekstiä tietyn muodon mukaisesti. Ohjelmoijat käyttävät tätä käytäntöä helpottaakseen työtänsä ja varmistaakseen oikean tulostuksen.

## Kuinka tehdä:
```Haskell
length "Hello World!"
```
Tuloste: `12`

```Haskell
length "Moi!"
```
Tuloste: `4`

## Syväsukellus:
(1) Merkkijonon pituuden selvittäminen on ollut tärkeä osa ohjelmointia jo pitkään. Joissakin ohjelmointikielissä tähän tarkoitukseen on omat valmiit funktiot, kuten Pythonin `len()` tai JavaScriptin `str.length`.
(2) On myös mahdollista laskea merkkijonon pituus manuaalisesti käymällä läpi jokainen merkki ja lisäämällä laskuriin yksi jokaisen kohdalla. Tämä kuitenkin vaatii enemmän koodin määrää ja on alttiimpi virheille.
(3) Haskellissa merkkijonon pituuden selvittämiseen käytetään `length` funktiota, joka palauttaa kokonaisluvun merkkijonon pituudesta. Tämän takana on tarkkaan optimoitu algoritmi, joten se on tehokkaampi vaihtoehto verrattuna manuaaliseen laskentaan.

## Katso myös:
- [Haskellin dokumentaatio merkkijonojen pituuden selvittämisestä](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:length)
- [Haskellin virallinen sivusto](https://www.haskell.org/)