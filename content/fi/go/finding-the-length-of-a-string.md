---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "Go: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

## Kuinka

Koodin pituus on tärkeä tieto monessa ohjelmoinnin käyttötarkoituksessa. Onneksi Go-kieli tarjoaa helpon tavan löytää merkkijonon pituus. 

```Go 
package main

import "fmt"

func main() {
	s := "Tämä on merkkijono"
	fmt.Println("Merkkijonon pituus on", len(s))
}
```

Tuloste: `Merkkijonon pituus on 20`

## Syvempi sukellus

Merkkijonon pituus löytyy käyttämällä Go-kielen `len()` -funktiota, joka palauttaa merkkijonon merkkien lukumäärän. Merkkien lukumäärä sisältää myös välilyönnit ja muut erikoismerkit.

Toinen tapa löytää merkkijonon pituus on käyttämällä `for`-silmukkaa ja laskemalla merkkien lukumäärä itse. Tämä lähestymistapa vaatii enemmän koodia, mutta antaa enemmän kontrollia ja mahdollisuuden tehdä lisätoimenpiteitä merkkijonon kanssa.

```Go
package main

import "fmt"

func main() {
	s := "Tämä on merkkijono"
	count := 0
	for range s {
		count++
	}
	fmt.Println("Merkkijonon pituus on", count)
}
```

Tuloste: `Merkkijonon pituus on 20`

## Katso myös

- [Go-opas - Merkkijonot](https://golang.org/doc/effective_go.html#strings)
- [Go-kirjasto - strings](https://golang.org/pkg/strings/)