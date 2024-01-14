---
title:    "Go: Kuvion mukaisen merkin poistaminen"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi
Monesti Go-ohjelmoinnin aikana voi ilmetä tarve poistaa merkkejä tietyltä kaavalta. Tässä blogikirjoituksessa kerromme, miksi tällaista toimintoa saatetaan tarvita ja miten se voidaan toteuttaa.

## Miten
Go-ohjelmoinnissa merkkejä voidaan poistaa helposti erilaisten kaavojen avulla. Esimerkkikoodissa käytämme funktiota "ReplaceAllString" ja sen avulla poistamme kaikki pisteet merkkijonosta "abc.def.ghi". Alla näet koodin ja siihen liittyvän outputin:

```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	// Luo uusi säännöllinen lauseke
	re := regexp.MustCompile(`\.`)

	// Käytä ReplaceAllString funktiota poistaaksesi pisteet merkkijonosta
	result := re.ReplaceAllString("abc.def.ghi", "")

	// Tulosta lopputulos
	fmt.Println(result)
	
	// Output:
	// abcdefghi
}
```

## Syvällinen tarkastelu
Merkeillä poistaminen voi joskus olla tarpeellista esimerkiksi tietojen käsittelyssä tai merkkijonojen muokkaamisessa. Go-ohjelmoinnissa tähän on tarjolla monipuolisia työkaluja, kuten esimerkiksi säännölliset lausekkeet. Näiden avulla on mahdollista poistaa tai korvata merkkejä halutulla tavalla.

## Katso myös
- [Go:n virallinen sivusto](https://golang.org/)
- [Go-kieltä käsittelevä blogi](https://blog.golang.org/)
- [Go-ohjelmoinnin opas](https://gobyexample.com/)

Kiitos lukemisesta ja hauskoja Go-ohjelmointihetkiä!