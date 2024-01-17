---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Elm: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Säännölliset lausekkeet ovat tekstin käsittelyyn tarkoitettuja työkaluja, jotka auttavat löytämään ja muokkaamaan tietynlaisia merkkijonoja. Ohjelmoijat käyttävät niitä esimerkiksi datan etsimiseen ja validointiin sekä tekstien korvaamiseen. Säännölliset lausekkeet säästävät aikaa ja vaivaa, ja ovat erittäin hyödyllisiä monissa eri ohjelmointiprojekteissa.

## Kuinka?

Elm-kielellä säännöllisiä lausekkeita käytetään `Regex`-kirjaston avulla. Alla on esimerkki, miten voit esimerkiksi tarkistaa, onko annettu merkkijono validi sähköpostiosoite:

```Elm
import Regex exposing (..)

isValidEmail : String -> Bool
isValidEmail str =
    case Regex.match (regex "[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,3}$") str of
        Ok _ ->
            True

        Err _ ->
            False
```

Tässä koodissa käytämme `match`-funktiota, joka ottaa parametreikseen säännöllisen lausekkeen ja tarkistettavan merkkijonon. Jos merkkijono täsmää annettuun regexiin, palauttaa funktio `Ok`-arvon, muuten `Err`-arvon. Voit myös käyttää regexiä esimerkiksi datan hakuun tai korvaamiseen, kuten seuraavassa esimerkissä:

```Elm
import Regex exposing (..)

replaceAll : String -> String -> String -> String
replaceAll old new str =
    Regex.replace (regex old) (\_ -> new) str

> replaceAll "elämä" "maailma" "Tämä on ihana elämä!"
"Tämä on ihana maailma!"
```

## Syvemmälle

Termi "säännöllinen lauseke" tulee matematiikasta, ja niitä on käytetty jo yli 60 vuoden ajan ohjelmoinnissa. Monet kielet tarjoavat sisäänrakennetun tavan käyttää säännöllisiä lausekkeita, mutta Elm-kielessä ne on toteutettu kirjastona. Muutama vaihtoehto Regex-kirjastolle Elm-kielessä on esimerkiksi `re` ja `regex-string` -kirjastot.

Säännöllisten lausekkeiden toteutus perustuu tekstin parsimiseen ja vertailuun sallittuihin merkkijonoihin. Ne voivat olla hyödyllisiä myös esimerkiksi tietokantojen käsittelyssä tai tiedoston muokkaamisessa.

## Katso myös

- [Elm-kirjasto: Regex](https://package.elm-lang.org/packages/elm/regex/latest/)
- [Blogikirjoitus säännöllisten lausekkeiden käytöstä Elm-kielessä](https://www.jancona.com/posts/2016-12-13-elm-regex.html)