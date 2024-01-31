---
title:                "Säännöllisten lausekkeiden käyttö"
date:                  2024-01-19
simple_title:         "Säännöllisten lausekkeiden käyttö"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions (regex) ovat kuvioiden haku- ja manipulointimenetelmiä, joita käytetään tekstin käsittelyssä. Ohjelmoijat käyttävät regexejä, koska ne tekevät monimutkaisten merkkijonojen etsinnästä ja valvonnasta tehokasta ja joustavaa.

## How to:
```
package main

import (
	"fmt"
	"regexp"
)

func main() {
	// Esimerkki: Sähköpostiosoitteen validoiminen regexillä
	emailRegex := regexp.MustCompile(`^[a-z0-9._%+\-]+@[a-z0-9.\-]+\.[a-z]{2,4}$`)
	email := "esimerkki@domain.fi"

	match := emailRegex.MatchString(email)
	fmt.Println("Onko sähköpostiosoite kelvollinen:", match)
	// Tulostus: Onko sähköpostiosoite kelvollinen: true

	// Esimerkki: Merkkijonojen etsiminen ja korvaaminen
	text := "Go on mahtava kieli. Go-kehittäjät rakastavat Go:n selkeyttä."
	regex := regexp.MustCompile(`Go`)
	newText := regex.ReplaceAllString(text, "Golang")
	fmt.Println(newText)
	// Tulostus: Golang on mahtava kieli. Golang-kehittäjät rakastavat Golang:n selkeyttä.
}
```

## Deep Dive
Regex syntaksi on kehittynyt useiden vuosikymmenten aikana, Unix-järjestelmästä alkaen. Vaikka Go:n regex-kirjasto ei ole yhtä tehokas kuin joissakin muissa kielissä, kuten Perlissä, se sopii useimpiin perustarpeisiin. Vaihtoehtoisia kirjastoja, kuten RE2, löytyy suorituskykyä vaativiin tehtäviin. Go:n regex-kirjasto tekee kompromisseja nopeuden ja muistinkäytön suhteen, mutta on turvallinen reentrancy- ja backtracking-ongelmien kannalta.

## See Also
- Go:n regex-paketin dokumentaatio: https://pkg.go.dev/regexp
- Google's RE2 regex-moottori GitHubissa: https://github.com/google/re2
- Regexperin visuaalinen regex-testeri: https://regexper.com/
