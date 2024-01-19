---
title:                "HTTP-pyynnön lähettäminen"
html_title:           "Bash: HTTP-pyynnön lähettäminen"
simple_title:         "HTTP-pyynnön lähettäminen"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

HTTP-pyyntö on tapa hakea tietoa palvelimelta. Ohjelmoijat lähettävät niitä tietokoneohjelmissa tietojen hakemiseksi ja päivittämiseksi palvelimilta.

## Kuinka:

Tässä näyttämme, kuinka lähettää HTTP GET -pyyntö Go-kielellä:

```Go
package main

import (
	"io/ioutil"
	"log"
	"net/http"
)

func main() {
	resp, err := http.Get("http://example.com/")
	if err != nil {
		log.Fatal(err)
	}

	defer resp.Body.Close()
	
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		log.Fatal(err)
	}
	println(string(body))
}
```

Tämän suorittaminen näyttää palvelimen vastauksen.

## Syventävä osuus:

HTTP-pyynnöt ovat olleet www:n perussarakkeita sen perustamisesta lähtien 1980-luvulla. Ne ovat vanhimpia ja yleisimpiä tapoja kommunikoida palvelimien kanssa. Mutta on olemassa vaihtoehtoja, kuten GRPC ja GraphQL, jotka tarjoavat enemmän ominaisuuksia ja tehokkuutta tietyissä tilanteissa.

Go:n `net/http` -paketti on erittäin tehokas sekä suoraviivainen HTTP-pyyntöjen lähettämiseen. Sen avulla voit myös hallita asioita kuten virheenkäsittely, pyynnön otsikot ja enemmän.

## Katso myös:

1. Go:n virallinen dokumentaatio HTTP-pyynnöistä: https://golang.org/pkg/net/http/
2. Opas HTTP-pyyntöjen tekemiseen Go:ssa: https://gobyexample.com/http-clients
3. GRPC:n käyttö Go:ssa: https://grpc.io/docs/languages/go/
4. GraphQL:n käyttö Go:ssa: https://github.com/graph-gophers/graphql-go