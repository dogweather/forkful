---
title:                "Elm: Kuviota vastaavien merkkien poistaminen"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi: Miksi poistaisit merkkejä, jotka vastaavat tiettyä kaavaa?

Usein ohjelmoinnissa on tarpeen käsitellä tietynlaisia merkkijonoja ja joskus näissä merkkijonoissa on ylimääräistä tai ei-toivottua tietoa. Tässä tapauksessa poistamalla merkkejä, jotka vastaavat tiettyä kaavaa, voit helposti puhdistaa ja muokata merkkijonoja halutulla tavalla. Tämä on erityisen kätevää, jos sinun tarvitsee käsitellä suuria määriä dataa tai muokata sitä automaattisesti.

## Miten: Esimerkkejä koodista ja tulosteista ```Elm ...```

Jos haluat poistaa merkkejä, jotka vastaavat tiettyä kaavaa, voit käyttää Elm-kielestä löytyviä valmiita funktioita, kuten `String.filter`. Tämä funktio ottaa parametreikseen merkkijonon ja funktion, joka määrittelee, mitkä merkit halutaan poistaa. Tässä esimerkissä poistetaan kaikki numerot merkkijonosta "ABCD1234":

```
import String exposing (filter)

filteredString : String
filteredString =
    filter (\char -> not (Char.isDigit char)) "ABCD1234"

-- 'filteredString' on nyt "ABCD"
```

Toinen tapa poistaa merkkejä on käyttämällä `String.replace`-funktiota, joka korvaa tietyn kaavan vastaavat merkit halutulla merkkijonolla. Esimerkiksi voit korvata kaikki numerot tyhjällä merkkijonolla "":

```
import String exposing (replace)

replacedString : String
replacedString =
    replace "1234" "" "ABCD1234"

-- 'replacedString' on nyt "ABCD"
```

## Syvempi sukellus: Tietoa merkkien poistamisesta vastaavan kaavan avulla

Edellä mainittujen esimerkkien lisäksi Elm-kielessä on muitakin tapoja poistaa merkkejä vastaavasta kaavasta. Yksi näistä on käyttää `Regex.replace`-funktiota, joka hyödyntää säännöllisiä lausekkeita (regex) merkkijonon muokkaamiseen. Tässä esimerkissä poistetaan kaikki välilyönnit merkkijonosta "Hello World":

```
import Regex exposing (replace)

regexResult : Result String String
regexResult =
    replace (Regex.regex " ") (\match -> "") "Hello World"

-- 'regexResult' on nyt Result.Ok "HelloWorld"
```

On myös hyvä huomata, että merkkejä voidaan poistaa myös ulkoisista lähteistä, kuten tietokannoista tai tiedostoista. Tässä tapauksessa voit käyttää `Task.attempt`-funktiota, joka avaa ulkoisen lähteen, poistaa merkkejä vastaavan kaavan mukaisesti ja palauttaa tuloksen.

## Katso myös

- [Elmin String-moduuli](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Säännölliset lausekkeet Elmissä](https://package.elm-lang.org/packages/elm/regex/latest/)
- [Task-moduuli Elmissä](https://package.elm-lang.org/packages/elm/core/latest/Task)