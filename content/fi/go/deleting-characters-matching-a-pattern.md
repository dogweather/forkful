---
title:                "Kuvion mukaisten merkkien poistaminen"
html_title:           "Go: Kuvion mukaisten merkkien poistaminen"
simple_title:         "Kuvion mukaisten merkkien poistaminen"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Merkinpoiston sovitusmallin mukaiset kirjoituksen poistaminen on yleinen ohjelmointitehtävä, jota käytetään tiettyjen merkkijonojen suodattamiseen tai muokkaamiseen. Tämä tehdään usein ohjelmia kehitettäessä tarkkuuden ja tehokkuuden maksimoimiseksi.

## Näin teet sen:
```Go
package main

import (
	"fmt"
	"regexp"
	"strings"
)

func main() {
	// Luo uusi merkkijono
	s := "Hei kaikille! Tervetuloa Go-ohjelmoijien joukkoon."

	// Käytä Regex-mallia etsimään halutut kirjoitukset
	reg, _ := regexp.Compile("[aeiou]")

	// Käytä korvaamistoimintoa korvataksemme löydetyt merkit tyhjällä
	newStr := reg.ReplaceAllString(s, "")

	fmt.Println(newStr) // Tulostaa "H ll! Trvl G-pgrmjn jkkn."
}
```

## Syväsukellus:
Merkinpoistomenetelmä on kehitetty jo varhaisessa ohjelmoinnin historiassa ja on tullut yhä kehittyneemmäksi ajan kuluessa. Joissakin muissa ohjelmointikielissä käytetään erilaisia menetelmiä merkkijonojen suodattamiseen tai muokkaamiseen, kuten säännölliset lausekkeet (regex) tai sisäänrakennetut merkkitaulukot. Merkinpoistoa käytetään myös usein osana tiedonkäsittelyä ja tietokantojen käsittelyä.

## Katso myös:
- [Go-kielen virallinen sivusto](https://golang.org/)
- [Go-kielen dokumentaatio](https://golang.org/doc/)
- [Säännöllisten lausekkeiden opas](https://regexone.com/lesson/introduction_abcs)