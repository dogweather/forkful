---
title:                "Go: Tarkista, onko hakemisto olemassa"
simple_title:         "Tarkista, onko hakemisto olemassa"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Miksi: Tarkastaa jos hakemisto on olemassa

Usein ohjelmoinnissa syntyy tilanteita, jossa tarvitaan tietää onko tietyssä hakemistossa olemassa tiettyä tiedostoa tai hakemistoa. Tähän tarpeeseen vastaamiseksi Go-kieltä on tarjolla monia erilaisia kirjastoja ja toimintoja, jotka mahdollistavat hakemistojen tarkistamisen.

## Miten: Koodiesimerkkejä ja tulostettuja tuloksia

```Go
package main

import (
	"os"
	"fmt"
)

func main() {
	// Tarkastaako hakemisto "data" on olemassa
	if _, err := os.Stat("./data"); os.IsNotExist(err) {
		fmt.Println("Hakemisto 'data' ei ole olemassa")
	} else {
		fmt.Println("Hakemisto 'data' on olemassa")
	}
}
```

Tässä esimerkissä käytetään os-kirjaston `Stat`-funktiota, joka palauttaa tiedon halutun polun tilasta. Jos hakemisto ei ole olemassa, tulostetaan viesti siitä. 

Tarkempi tieto hakemiston tilasta voidaan saada `FileInfo`-tietorakenteesta, joka palauttaa esimerkiksi hakemiston luomisajankohdan ja muokkausajankohdan. Esimerkki käyttäen tätä tietoa:

```Go
package main

import (
	"os"
	"fmt"
	"time"
)

func main() {
	// Tarkastaako hakemisto "data" on olemassa
	if fi, err := os.Stat("./data"); os.IsNotExist(err) {
		fmt.Println("Hakemisto 'data' ei ole olemassa")
	} else {
		fmt.Printf("Hakemisto 'data' on olemassa ja sitä on muokattu viimeksi %s\n", fi.ModTime().Format(time.RFC1123))
	}
}
```

Tämä tulostaisi esimerkiksi: `Hakemisto 'data' on olemassa ja sitä on muokattu viimeksi Tue, 21 Jan 2020 15:04:05 UTC`.

## Syvällisempi tarkastelu: Hakemistojen tarkistamisen lisätietoja

`Stat`-funktion lisäksi, Go-kielen `filepath`-paketti tarjoaa muitakin hyödyllisiä toimintoja hakemistojen tarkistamiseen. Näihin kuuluvat esimerkiksi `Walk`-funktio, joka mahdollistaa käymisen läpi kaikki alihakemistot ja niiden tiedostot tietystä polusta, sekä `IsAbs`-funktio, joka tarkistaa, onko annettu polku absoluuttinen vai suhteellinen.

## Katso myös

- [Go Language Specification - Os Package](https://golang.org/pkg/os/)
- [Go Language Specification - Filesystem Implementation](https://golang.org/pkg/os/#Filesystem_Implementations)
- [Go Language Specification - File Path Handling](https://golang.org/pkg/filepath/)