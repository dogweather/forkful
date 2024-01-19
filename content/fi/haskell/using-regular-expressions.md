---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Haskell: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Säännölliset lausekkeet ovat kaavoja, joita käytetään merkkijonojen etsimiseen ja korvaamiseen. Ohjelmoijat käyttävät niitä, koska ne säästävät aikaa ja tekevät koodista selkeää.

## Näin tehdään:

Käytä `Text.Regex.Posix`-moduulia merkkijonojen käsittelemiseen. Tässä on esimerkki säännöllisen lausekkeen käytöstä.

```Haskell
import Text.Regex.Posix

lahtoteksti = "Hello, Haskell!"

lahtoteksti =~ "Haskell" :: Bool
```

Jos "Haskell" löytyy `lahtotekstistä`, tuloksena on `True`.

## Syvällisemmin:

### Historia:

Säännölliset lausekkeet tulivat tunnetuiksi ed-vi-tekstieditorin myötä 1970-luvulla. Haskell-ohjelmointikielessä niiden käyttöön otettiin `Text.Regex`-kirjasto.

### Vaihtoehdot:

`Text.Regex.Posix` on suosittu, mutta tilanteesta riippuen voit käyttää `Text.Regex.PCRE` tai `Text.Regex.TDFA` -kirjastoja.

### Toteutus:

Haskellissa säännölliset lausekkeet käsitellään monadisen tyylin mukaisesti, joka takaa laskennan laiskuuden ja parantaa suorituskykyä.

## Tutustu myös:

[Learn You a Haskell for Great Good](http://learnyouahaskell.com/) on hyvä resurssi Haskelliin ja sen ominaisuuksiin.

[Haskell Text.Regex.Posix documentation](https://hackage.haskell.org/package/regex-posix-0.96.0.0/docs/Text-Regex-Posix.html) sisältää kaiken tiedon mitä tarvitset `Text.Regex.Posix`-kirjaston käyttöön.