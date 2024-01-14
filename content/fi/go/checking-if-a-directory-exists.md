---
title:    "Go: Tarkistetaan, onko hakemistoa olemassa"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Miksi

Tiedostojen ja kansioiden hallinta on tärkeä osa ohjelmointia. Usein tarvitsemme tarkistamaan, onko tiettyä hakemistoa olemassa ja toimimaan sen mukaisesti. Tässä blogikirjoituksessa käsittelemme, miten tämä voidaan toteuttaa Go-kielellä.

## Miten tehdä

```Go
package main

import (
	"os"
	"fmt"
)

func main() {
	_, err := os.Stat("hakemisto")

	if err != nil {
		fmt.Println("Hakemistoa ei löytynyt")
		return
	}

	fmt.Println("Hakemisto löytyy")
}
```

Koodissa luodaan hakemisto nimeltä "hakemisto" ja sitten tarkistetaan sen olemassaolo `os.Stat()` -funktion avulla. Funktio palauttaa osan ja virheen. Jos osa on `nil`, se tarkoittaa, että hakemisto on olemassa. Jos virhe ei ole `nil`, se tarkoittaa, että hakemistoa ei ole olemassa. Tässä tapauksessa tulostetaan "Hakemistoa ei löytynyt".

## Syvemmälle

`os.Stat()` -funktiossa käytetään `PathInfo` -rakennetta, joka palauttaa tiedot tiedostosta tai hakemistosta. Tätä rakennetta voidaan käyttää myös tiedoston koon, muokkauspäivän ja muiden tietojen tarkistamiseen. `os.Stat()` -funktio on myös hyödyllinen tiedostojen käsittelyssä ja siitä voidaan löytää lisätietoa Go:n dokumentaatiosta.

## Katso myös

- [Go:n dokumentaatio - os paketti](https://golang.org/pkg/os/)
- [Go Tutorial - Tiedostojen ja kansioiden käsittely](http://www.golangprograms.com/go-language/file-handling-in-go-language.html)