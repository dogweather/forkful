---
title:                "Tilapäisen tiedoston luominen"
html_title:           "Arduino: Tilapäisen tiedoston luominen"
simple_title:         "Tilapäisen tiedoston luominen"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Luodaan väliaikainen tiedosto kun tarve väliaikaisesti tallentaa tietoa, esimerkiksi logit tai tilapäiset muokkausjäljet. Tämä säästää muistia ja auttaa ohjelmia,koska tiedot eivät kulje edestakaisin muistin ja levyn välillä.

## Näin teet:

```Go
package main

import (
	"io/ioutil"
	"fmt"
)

func main() {
	tempFile, err := ioutil.TempFile("temp", "progr-*.txt")
	if err != nil {
		panic(err)
	}

	fmt.Println("Väliaikainen tiedosto luotu:", tempFile.Name())
}
```
Kun ajat tämän koodin, se luo temp-nimisen hakemiston ja siellä on uusi tiedosto nimeltä "progr-*.txt".

```Output
Väliaikainen tiedosto luotu: /temp/progr-63kf8f2n.txt
```
## Syvempi silmäys

Ei ole tarkkaa ajankohtaa kun luotiin ensimmäinen väliaikainen tiedosto, mutta luultavasti vuosikymmeniä sitten. Vaihtoehtoisia tapoja tiedon tallentamiseen ovat muun muassa väli- tai jaetut muistit tai verkkoyhteydet paikallisten tiedostojen sijaan.

Go:n `ioutil.TempFile()`-funktio käyttää yksilöllisiä muodostimia tekemään aina yksilöllisen tiedoston. Tämä minimoi törmäyksen mahdollisuuden. Tiedostot korjataan automaattisesti Go:n jätekeräimen toimesta, kun niitä ei enää tarvita.

## Katso myös:

- Go:n viralliset dokumentit: <https://golang.org/pkg/io/ioutil/#TempFile>
- Erilaisia menetelmiä tiedon tallentamiseen Go:ssa: <https://gobyexample.com/temporary-files-and-directories>
- Lisätietoja Go:n jätekerääjästä: <https://blog.golang.org/ismmkeynote>