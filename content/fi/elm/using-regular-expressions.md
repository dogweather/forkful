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

## Miksi käyttää reguläärisiä lausekkeita?

Reguläärisiä lausekkeita käytetään usein ketterissä ohjelmointikielissä, kuten Elm, joiden avulla voimme käsitellä ja manipuloida tekstidataa. Ne ovat hyödyllisiä esimerkiksi tekstin etsimisessä, korvaamisessa ja validoinnissa. Niiden avulla voimme käsitellä monimutkaisia hakuehtoja ja säästää paljon aikaa ja vaivaa.

## Näin käytät reguläärisiä lausekkeita Elmissä
```elm
-- Etsitään sana "kissaeläin" tekstistä
Regex.find (Regex.regex "kissaeläin") "Tässä on muutama kissaeläin: kissa, koira, hamsteri"

-- Tulostaa: Just (Ok (Regex.Match { match = "kissaeläin", submatches = [], index = 27, number = 1, namedSubmatches = Dict.empty }))
```

```elm
-- Korvataan sana "maailma" "universumi"
Regex.replace (Regex.regex "maailma") "universumi" "Tervetuloa uuteen maailmaan"

-- Tulostaa: Just (Ok "Tervetuloa uuteen universumiin")
```

Regulääriset lausekkeet ovat myös käteviä tarkistamaan esimerkiksi sähköpostiosoitteen tai puhelinnumeron muotoa.

```elm
-- Tarkistetaan, onko annettu merkkijono kelvollinen sähköpostiosoite
Regex.contains (Regex.regex "[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,4}") "esimerkki@domain.com"

-- Tulostaa: Just (Ok True)
```

## Syvemmälle reguläärisiin lausekkeisiin

Regulääriset lausekkeet perustuvat useisiin erikoismerkkeihin ja symboleihin, jotka määrittelevät tietyt haku- ja korvausmallit. Esimerkiksi symbolit ".", "+", "?" ja "*" tarkoittavat erilaisia sääntöjä merkkijonon osien sijoittamisessa.

Monet ohjelmointikielillä toimivat regulääriset lausekkeet perustuvat POSIX-määrityksiin, jotka määrittelevät tarkat hakuehdot ja säännönsiirtymät.

Jos haluat oppia lisää reguläärisistä lausekkeista ja niiden käytöstä Elmissä, voit tarkistaa virallisen dokumentaation osoitteesta https://package.elm-lang.org/packages/elm/regex/latest.

## Katso myös

- [Elm - virallinen verkkosivusto](https://elm-lang.org/)
- [Elm - suomenkielinen dokumentaatio](https://guide.elm-lang.org/)