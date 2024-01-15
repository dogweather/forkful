---
title:                "Tekstitiedoston lukeminen"
html_title:           "Go: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Miksi lukea tiedostoa Go-ohjelmoinnilla? Tämä artikkeli kertoo kuinka tekstitiedosto voidaan lukea Go-kielellä ja tarjoaa syvempää tietoa aiheesta.

## Kuinka

Käyttäjät voivat käyttää Go-ohjelmointikieltä lukemaan erilaisia tiedostoja, mukaan lukien teksti-tiedostoja. Tämä tapahtuu käyttämällä *os* ja *io/ioutil* paketteja. Seuraava koodiesimerkki näyttää kuinka tekstitiedosto voidaan lukea ja tulostaa konsoliin.

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"os"
)

func main() {
	// Avaa haluttu tiedosto
	file, err := os.Open("tekstitiedosto.txt")
	if err != nil {
		fmt.Println(err)
	}
	defer file.Close()

	// Lukee tiedoston sisällön ([]byte muodossa)
	content, err := ioutil.ReadAll(file)
	if err != nil {
		fmt.Println(err)
	}

	// Tulostaa tiedoston sisällön konsoliin
	fmt.Println(string(content))
}
```

Koodin suorittamisen jälkeen, konsoliin tulostuu tekstitiedoston sisältö. Huomaa, että *err* muuttujaa käytetään virheiden käsittelyssä. On tärkeää huolehtia mahdollisista virhetilanteista tiedostoa luettaessa.

## Syvällisempää tietoa

*os* ja *io/ioutil* paketit tarjoavat useita metodeja tiedoston käsittelyyn. Esimerkiksi, *ioutil.ReadFile()* voi lukea koko tiedoston sisällön *[]byte* muodossa, kun taas *ioutil.ReadFileToString()* muuttaa tiedoston sisällön *string* muotoon. Lue lisää näistä paketeista Go:n virallisesta dokumentaatiosta. 

## Katso myös

- [Go:n virallinen dokumentaatio](https://golang.org/doc/)
- [Go-paketit tiedostojen käsittelyyn](https://golang.org/pkg/os/)