---
title:                "Onko hakemisto olemassa? Tarkistaminen"
date:                  2024-01-20T14:56:40.202417-07:00
html_title:           "Gleam: Onko hakemisto olemassa? Tarkistaminen"
simple_title:         "Onko hakemisto olemassa? Tarkistaminen"

category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä ja Miksi?)
Tarkistamme, onko kansio olemassa, koska tiedämme, ettei mielenkiintoisia asioita voi tehdä olemattomilla kansioilla. Se on peruskysely, jonka avulla vältetään virheitä, kun käsittelemme tiedostoja ja kansioita.

## How to: (Kuinka tehdä:)
```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	directory := "/path/to/directory"

	if _, err := os.Stat(directory); os.IsNotExist(err) {
		fmt.Printf("Kansiota ei löydy: %s\n", directory)
	} else {
		fmt.Printf("Kansio löytyy: %s\n", directory)
	}
}
```
**Esimerkkitulostus:**
```
Kansiota ei löydy: /path/to/directory
```
Tai jos kansio on olemassa:
```
Kansio löytyy: /path/to/directory
```

## Deep Dive (Syväsukellus)
Ennen `os.Stat` funktiota, ohjelmoijien piti selvitä kansion olemassaolo käsin avaten kansioita ja tarkistamalla virheitä. `os.Stat` ja `os.IsNotExist` ovat nyt standarditavat Go:ssa tehdä tämä tarkistus. Ne ovat olleet osa Go:ta sen varhaisten versioiden lähtien, ja ne hoitavat eri alustoilla toimimisen. Vaihtoehtoina `os.Stat` funktiolle voidaan käyttää kolmansien osapuolien kirjastoja, kuten `github.com/pkg/errors`, jotka tarjoavat lisäominaisuuksia virheenkäsittelyyn, mutta useimmissa tapauksissa Go:n vakio-kirjastot riittävät ja ovat tehokkaita.

## See Also (Katso Myös)
- Go:n virallinen dokumentaatio `os.Stat`: https://golang.org/pkg/os/#Stat
- Go:n virallinen dokumentaatio virheiden käsittelyyn: https://golang.org/doc/effective_go.html#errors
- Go by Example, kansiotiedot: https://gobyexample.com/directories
