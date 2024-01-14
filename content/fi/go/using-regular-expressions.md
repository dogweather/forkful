---
title:                "Go: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita?

Säännölliset lausekkeet ovat hyödyllinen työkalu Go-ohjelmoinnissa, sillä ne mahdollistavat tietyn tekstin tai merkkijonon etsimisen ja muokkaamisen halutulla tavalla. Esimerkiksi jos tarvitset ohjelmassasi tietynlaista merkkijonoa, voit käyttää säännöllisiä lausekkeita sen löytämiseen ja käsittelyyn.

## Miten käyttää säännöllisiä lausekkeita Go:lla?

Käytännössä säännöllisiä lausekkeita käytetään Go-ohjelmissa usein string-paketin Regexp-tyypin avulla. Alla on esimerkki koodista, joka hakee listasta kaikki merkkijonot, jotka sisältävät numeron:

```
Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	merkkijonolista := []string{"Merkkijono1", "Merkkijono2", "Merkkijono3 123", "Merkkijono4"}

	regex, _ := regexp.Compile("\\d+")

	for _, merkkijono := range merkkijonolista {
		if regex.MatchString(merkkijono) {
			fmt.Println(merkkijono)
		}
	}
}
```

Tämä koodi tuottaa seuraavanlaisen tulosteen:

```
Merkkijono3 123
```

Ensimmäisellä rivillä tuodaan käyttöön tarvittavat paketit ja muuttujat, jotka alustetaan toisella rivillä. Kolmannella rivillä säännöllinen lauseke tallennetaan muuttujaan käyttämällä Regexp.Compile-funktiota. Neljännellä ja viidennellä rivillä käydään läpi merkkijonolista for-silmukalla ja tarkistetaan jokainen merkkijono säännöllisen lausekkeen avulla. Jos merkkijono sisältää numeron, se tulostetaan.

## Syvempää tietoa säännöllisten lausekkeiden käytöstä Go-ohjelmissa

On hyvä tiedostaa, että säännölliset lausekkeet toimivat tietyllä tavalla ja niiden käytön oppiminen vaatii hieman harjoittelua ja perehtymistä aiheeseen. Voit esimerkiksi käyttää online-sivustoja, kuten RegExr, harjoitellaksesi säännöllisten lausekkeiden luomista ja testaamista. Go:n string-paketin dokumentaatio sisältää myös tarkempia tietoja säännöllisten lausekkeiden käytöstä.

## Katso myös
- Go string-paketin dokumentaatio: https://golang.org/pkg/strings/
- RegExr: https://regexr.com/