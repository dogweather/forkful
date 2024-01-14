---
title:                "Go: Kirjailu tietokoneohjelmointi: Kaavan mukaisten merkkien poistaminen"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Päätöntä näppäinten naputtelua kaipaavan on vaikea ymmärtää, miksi joku haluaisi poistaa tietyn kuviomäärityksen mukaiset merkit Go-ohjelmoinnissa. Kyseessä saattaa kuitenkin olla tarpeellinen toiminto datan käsittelyssä, jota käsitellään tulevassa osiossa.

## Miten

Go-kielellä on useita tapoja poistaa merkkejä kuviomäärityksen perusteella. Tässä on yksi esimerkki, jossa poistetaan kaikki välilyönnit annetusta merkkijonosta ja tulostetaan lopputulos.

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "Go-kieli on loistava!"
	fmt.Println(strings.ReplaceAll(str, " ", ""))
}

// Output: Gokielionloistava!
```

On tärkeää muistaa, että tällä tavoin voidaan poistaa vain yksi merkkijono kerrallaan. Jos halutaan poistaa esimerkiksi sekä välilyönnit että välimerkit, täytyy käyttää useampia ReplaceAll-funktioita tai esimerkiksi Regular Expression -ratkaisua.

## Syvempi sukellus

Mikäli halutaan ymmärtää paremmin, miten merkkien poistaminen kuvion perusteella tapahtuu Go-kielessä, täytyy perehtyä Regular Expression (reguläärilausekkeet) -käsitteeseen. Tämä mahdollistaa monimutkaisempien kuviomääritysten käytön ja tarkempaan kontrollointiin poistettavien merkkien suhteen.

## Katso myös

- [Go-kielin virallinen verkkosivusto](https://golang.org/)
- [Regular Expression -tutoriaali](https://regexone.com/)
- [Go-kieleen perehtyminen Askolan kunnan avoimen datan avulla](https://github.com/askolanvapaakaupunki/askola-opendata)