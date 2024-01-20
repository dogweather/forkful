---
title:                "Verkkosivun lataaminen"
html_title:           "C#: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?

Web-sivun lataaminen tarkoittaa datan siirtoa palvelimelta paikalliseen järjestelmään. Ohjelmoijat tekevät tämän tiedon keräämisen tai jakamisen vuoksi.

## Kuinka tehdään:

```Go
package main

import (
	"io"
	"net/http"
	"os"
)

func main() {
	res, err := http.Get("http://example.com")
	if err != nil {
		panic(err)
	}
	defer res.Body.Close()
	
	file, err := os.Create("example.html")
	if err != nil {
		panic(err)
	}
	defer file.Close()
	
	_, err = io.Copy(file, res.Body)
	if err != nil {
		panic(err)
	}
}
```

Tässä on esimerkki koodin tulosteesta:

```Go
"Tiedosto 'example.html' tallennettu onnistuneesti."
```

## Syvä sukellus:

Ladataan web-sivuja alkuun responsiivisen web-suunnittelun aikakaudella. Go-ohjelmointikieli tekee tästä yksinkertaisen standardikirjastonsa ansiosta. Vaihtoehtoina on käyttää selaimen automatisoituja työkaluja, kuten Puppeteer, tai pilvipohjaisia ratkaisuja, kuten import.io. Mutta Go tarjoaa tehokkaan, vähäisen koodin ja monikäyttöisen ratkaisun.

Gon http.Get() -funktio hakee URL:än ja palauttaa vastauksen. io.Copy()-toiminto kopioidaan sivun sisältö paikalliseen tiedostoon.

## Katso myös:

* Tutustu Go: n viralliseen http-pakettiin: https://golang.org/pkg/net/http/
* Go-kielen dokumentaatio: https://golang.org/doc/
* Deep Dive into Go's net/http package: https://medium.com/rungo/understanding-the-net-http-package-in-go-30e4ba6d83e9