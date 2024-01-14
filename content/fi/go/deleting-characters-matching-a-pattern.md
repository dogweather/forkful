---
title:                "Go: Mallia vastaavien merkkien poistaminen"
simple_title:         "Mallia vastaavien merkkien poistaminen"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Jotkut ohjelmoijat saattavat tarvita poistaa merkkejä, jotka vastaavat tiettyä mallia. Tämä voi olla hyödyllistä esimerkiksi tietojen puhdistamisessa tai tiettyjen merkkitietojen etsimisessä. Tässä artikkelissa kerromme, miten tämä voidaan toteuttaa Go-ohjelmointikielellä.

## Miten tehdä

Voit poistaa merkkejä, jotka vastaavat tiettyä mallia, käyttämällä "regexp" -pakettia. Tämä paketti tarjoaa laajan valikoiman työkaluja, joita voit käyttää merkkihakuihin ja korvaamiseen.

Esimerkiksi voimme luoda yksinkertaisen ohjelman, joka poistaa kaikki numerot sisältävät merkit merkkijonosta:

```Go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    // Luodaan säännöllinen lauseke, joka vastaa kaikkia numeroita
    regex := regexp.MustCompile("[0-9]+")

    // Testimerkkijono
    str := "Tämä on esimerkkimerkkijono, jossa on 123 numeroa."

    // Käytetään FindAllString -funktiota löytämään kaikki numerot
    matches := regex.FindAllString(str, -1)

    // Tulostetaan alkuperäinen merkkijono ja numerot sen sisällä
    fmt.Println("Alkuperäinen merkkijono:", str)
    fmt.Println("Numerot:", matches)

    // Poistetaan numerot merkkijonosta ja tulostetaan puhdistettu versio
    cleanStr := regex.ReplaceAllString(str, "")
    fmt.Println("Puhdistettu merkkijono:", cleanStr)
}
```

Tuloste:

```
Alkuperäinen merkkijono: Tämä on esimerkkimerkkijono, jossa on 123 numeroa.
Numerot: [123]
Puhdistettu merkkijono: Tämä on esimerkkimerkkijono, jossa on numeroa.
```

## Syvempää sukellusta

Säännöllisillä lausekkeilla on monia erilaisia käyttötarkoituksia ja ne voivat olla erittäin hyödyllisiä datan käsittelyssä ja analysoinnissa. Go:n "regexp" -paketti tarjoaa useita erilaisia toimintoja, joita voit käyttää monimutkaisempien mallien löytämiseen ja korvaamiseen.

## Katso myös

- [Go:n "regexp" -paketin virallinen dokumentaatio](https://golang.org/pkg/regexp/)
- [Säännölliset lausekkeet - Wikipedia (suomeksi)](https://fi.wikipedia.org/wiki/S%C3%A4%C3%A4nn%C3%B6llinen_lauseke)
- [15 Tavallista säännöllistä lauseketta](https://www.guru99.com/regular-expressions.html)