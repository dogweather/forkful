---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
date:                  2024-01-19
html_title:           "Arduino: Merkkijonon muuttaminen isoiksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Mikä on merkkijonojen suurentaminen ja miksi sitä tehdään? Turnaus suurentaa kaikki merkkijonon kirjaimet suuriksi kirjaimiksi. Sitä käytetään, kun haluamme yhdenmukaistaa tekstin esitystapaa, esimerkiksi käyttäjänimen tai avainsanojen käsittelyssä.

## How to:
Go-kielen standardikirjasto tarjoaa helpon tavan suurentaa merkkijonot. Käytä `strings`-pakettia ja sen `ToUpper`-funktiota:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	original := "moikka maailma!"
	capitalized := strings.ToUpper(original)
	fmt.Println(capitalized) // MOIKKA MAAILMA!
}
```

## Deep Dive
Turnaus Go-kielessä on suoraviivaista, mutta on hyvä ymmärtää, mitä kulissien takana tapahtuu. Kun merkkijono suurennetaan, jokainen Unicode-koodipiste mapataan vastaavaan suureen versioon, jos sellainen on olemassa. Historiallisesti eri ohjelmointikielet ovat toteuttaneet tämän eri tavoin, ja Go käyttää tehokasta Unicode-tietokantaa taatakseen oikeellisuuden.

Jos turnausta ei tarvita kaikille merkeille, voit käyttää `ToTitle` tai `ToUpperSpecial` funktioita. Esimerkiksi `ToTitle` suurentaa vain sanojen ensimmäiset kirjaimet. Funktio `ToUpperSpecial` sallii mukautetun kielikohtaisen suurentamisen.

Go:n standardikirjasto hoitaa erikoistapaukset toisistaan poikkeavissa kirjaimissa ja kielissä. Tämä osoittaa tehokkaasti erilaisia kirjaimistoja käyttävien yhteisöjen välillä.

## See Also
- Go `strings` paketti: https://pkg.go.dev/strings
- Unicode standardi: https://unicode.org/standard/standard.html
- Go blogi kirjoitus merkkijonon käsittelystä: https://blog.golang.org/strings
