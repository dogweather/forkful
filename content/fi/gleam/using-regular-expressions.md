---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Gleam: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita?

Säännölliset lausekkeet ovat tehokas tapa käsitellä merkkijonoja ja suorittaa tarkkoja hakuja ja muutoksia. Ne ovat erityisen hyödyllisiä, kun tietoa sisältävät merkkijonot ovat monimutkaisia tai vaihtelevia.

## Kuinka käyttää säännöllisiä lausekkeita Gleam-ohjelmoinnin kanssa?

```Gleam
import gleam/regexp

let content = "Tämä on esimerkki kaavasta 123-456-789"

let pattern = regexp.compile("[0-9]{3}-[0-9]{3}-[0-9]{3}")
let matches = regexp.matches(pattern, content)

// Tulostaa: [ 123-456-789 ]
```

"```regexp.compile()```" ottaa säännöllisen lausekkeen merkkijonona ja palauttaa käännöksen, jota voi sitten käyttää hakuun ja muokkaukseen.

## Syvällisempää tietoa säännöllisten lausekkeiden käytöstä

Säännöllisissä lausekkeissa voi käyttää erilaisia erikoismerkkejä ja symboleja, jotka tekevät hausta ja muokkauksesta joustavampaa. Esimerkiksi ```[0-9]``` etsii kaikki numerot 0:n ja 9:n väliltä ja ```{3}``` samassa lausekkeessa tarkoittaa, että haku kohdistuu kolmeen peräkkäiseen numeroon.

Säännöllisiä lausekkeita voi myös yhdistellä muihin Gleam-ohjelmointikielen ominaisuuksiin, kuten ```if```-lauseisiin ja ```let```-muuttujiin, jotta hakujen tulokset voidaan tallentaa ja käsitellä vaivattomasti.

## Katso myös

- Gleam-ohjelmointikielen viralliset dokumentit säännöllisistä lausekkeista: [https://gleam.run/book/regexes.html](https://gleam.run/book/regexes.html)
- RegExr, sivusto, jossa voi testata ja harjoitella säännöllisiä lausekkeita: [https://regexr.com/](https://regexr.com/)
- Gleam-ohjelmointikielen blogi, jossa on vinkkejä ja ideoita säännöllisten lausekkeiden käyttöön: [https://gleam.run/blog/](https://gleam.run/blog/)