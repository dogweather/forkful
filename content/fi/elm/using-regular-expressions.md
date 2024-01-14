---
title:                "Elm: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita?

Säännölliset lausekkeet ovat erittäin kätevä työkalu löytää ja muokata tiettyjä merkkijonoja tai tekstejä. Ne ovat hyödyllisiä esimerkiksi tietokantojen kyselyissä, lomakkeiden validoinnissa tai tiedostojen käsittelyssä. Säännölliset lausekkeet voivat säästää paljon aikaa ja vaivaa, sillä ne mahdollistavat tehokkaan ja joustavan tekstinmuokkauksen.

## Kuinka käyttää säännöllisiä lausekkeita Elm-ohjelmoinnissa?

Säännölliset lausekkeet ovat osa Elm:n yleistä `Regex`-kirjastoa, joten niiden käyttöönotto on hyvin yksinkertaista. Alla on esimerkkejä koodinpätkiä, jotka näyttävät kuinka säännöllisiä lausekkeita voi käyttää eri tilanteissa. 

### Tekstin löytäminen

```
--etsi sana "tervetuloa" annetusta merkkijonosta
let tervetuloa = Regex.fromString "tervetuloa"

--tarkista, löytyykö tekstistä sana "tervetuloa"
Regex.find tervetuloa "Tervetuloa uuteen kotiin!" == Just { matched = "tervetuloa"
, index = 0
, submatches = [] }
```

### Tekstin muokkaaminen

```
--muuta kaikki numerot tekstissä "*2" -merkkijonoksi
let muokkaus = Regex.fromString "[0-9]+"

--palauttaa tekstin "2*2*3"
Regex.replace muokkaus (\match -> "*2") "1*2*3" == "*2*2*3"
```

### Säännöllisten lausekkeiden yhdistäminen

```
--yhdistä sana "tervetuloa" ja "uusi koti"
let yhdistetty = Regex.fromString "(tervetuloa).*(uusi koti)"

--tarkista, löytyykö tekstistä molemmat sanat ja missä järjestyksessä
Regex.find yhdistetty "Tervetuloa uuteen kotiin!" == Just { matched = "Tervetuloa uuteen kotiin"
, index = 0
, submatches = [{ matched = "Tervetuloa"
                , index = 0 }
              , { matched = "uuteen kotiin"
                , index = 11 }]
```

## Syvempää tietoa säännöllisten lausekkeiden käytöstä

Säännöllisten lausekkeiden käyttö vaatii hieman tutustumista RegExp-määrittelyyn ja sen eri elementteihin. Tarkempaa tietoa löytyy esimerkiksi [MDN:n sivuilta](https://developer.mozilla.org/fi/docs/Web/JavaScript/Guide/Regular_Expressions) sekä [Elm:n virallisesta dokumentaatiosta](https://package.elm-lang.org/packages/elm/regex/latest/).

## Katso myös

- [Elm:n virallinen dokumentaatio](https://guide.elm-lang.org/)
- [MDN:n sivut säännöllisistä lausekkeista](https://developer.mozilla.org/fi/docs/Web/JavaScript/Guide/Regular_Expressions)