---
title:    "Go: Tarkistetaan onko hakemistoa olemassa"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi: Tarkistaa, onko hakemisto olemassa?

Hakemistojen tarkistaminen on tärkeä osa Go-ohjelmointia, sillä sen avulla voidaan varmistaa, että tarvittavat tiedostot ja hakemistot ovat olemassa ennen ohjelman suorittamista. Tämä välttää virheitä ja parantaa ohjelman suorituskykyä.

## Kuinka: Koodiesimerkkejä ja tulosnäkymät "```Go ... ```" koodilohkoissa.

```Go
package main

import "os"
import "fmt"

func main() {
    // Tarkista, onko hakemisto olemassa
    if _, err := os.Stat("/polku/hakemistoon/"); os.IsNotExist(err) {
        fmt.Println("Hakemisto ei ole olemassa")
    } else {
        fmt.Println("Hakemisto on olemassa")
    }
    
    // Luo uusi hakemisto
    os.Mkdir("/polku/uuteen/hakemistoon/", 0755)
}
```

Tuloste:
```
Hakemisto ei ole olemassa
```

## Syvempi sukellus: Tietoa hakemistojen tarkistamisesta

Go-kielessä hakemiston olemassaolon tarkistaminen tapahtuu os-paketin `Stat()` ja `IsNotExist()` funktsioiden avulla. Ensimmäinen funktio tarkistaa tiedoston tai hakemiston tilan, kun taas toinen funktio palauttaa boolean-arvon `true` tai `false` riippuen siitä, onko tiedosto tai hakemisto olemassa.

Jos hakemisto ei ole olemassa, se voidaan luoda `Mkdir()` tai `MkdirAll()` funktioilla. Ensimmäinen luo vain yhden tason hakemistoja, kun taas jälkimmäinen luo tarvittavan määrän alihakemistoja.

## Katso myös

- [Go os-paketin dokumentaatio](https://golang.org/pkg/os/)
- [Go Mkdir() ja MkdirAll() dokumentaatio](https://golang.org/pkg/os/#Mkdir)
- [Go-kielessä käytettävät Markdown-ominaisuudet](https://golang.org/cmd/gomarkdown/)