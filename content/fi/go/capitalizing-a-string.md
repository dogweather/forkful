---
title:                "Go: Merkkijonon suurentaminen"
simple_title:         "Merkkijonon suurentaminen"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Oletko koskaan miettinyt kuinka muuttaa merkkijonon ensimmäinen kirjain isoksi? Tämä on yleinen tarve monissa ohjelmointiprojekteissa, ja se voidaan toteuttaa helposti käyttäen Go-ohjelmointikieltä.

## Miten tehdä se

Go-kielessä on valmiina funktio, joka tekee tämän tehtävän puolestasi, nimeltään "strings.Title". Tämä funktio ottaa parametrina merkkijonon ja palauttaa sen ensimmäisen kirjaimen isona. Katso seuraava koodiesimerkki:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	name := "julius"
	fmt.Println(strings.Title(name))
}
```

Tämän koodin suorittaminen tuottaa seuraavan tulosteen: "Julius". Kuten näet, funktio strings.Title muutti merkkijonon ensimmäisen kirjaimen isoksi.

## Syvempi sukellus

Halutessasi voit tehdä tämän tehtävän myös itse käyttäen Go:n sisäänrakennettua Unicode-mekanismia. Tämä mahdollistaa myös kansainvälisesti käytettyjen merkkien oikeanlaisen käsittelyn. Voit käyttää seuraavaa koodilohkoa:

```Go
package main

import (
	"fmt"
	"unicode"
)

func capitalizeString(s string) string {
	r := []rune(s)
	r[0] = unicode.ToUpper(r[0])
	return string(r)
}

func main() {
	name := "söpö"
	fmt.Println(capitalizeString(name))
}
```

Tulostuu: "Söpö". Tässä koodissa käytetään ensin Go:n "rune"-tietotyyppiä, joka mahdollistaa merkkien käsittelyn Unicode-muodossa. Funktiossa capitalizeString ensimmäinen kirjain muutetaan isoksi ja palautetaan sitten merkkijonona.

## Katso myös

- [Go:n virallinen dokumentaatio merkkijonojen käsittelystä](https://golang.org/pkg/strings/)
- [Vinkkejä ja temppuja Go:n käytöstä merkkijonojen kanssa](https://www.sohamkamani.com/blog/2017/10/18/golang-strings-cheat-sheet/)

Toivottavasti tämä artikkeli auttoi sinua ymmärtämään kuinka helposti voit muuttaa merkkijonon ensimmäisen kirjaimen isoksi käyttäen Go-kieltä. Onnea koodaukseen!