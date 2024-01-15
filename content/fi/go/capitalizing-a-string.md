---
title:                "Merkkijonon ensimmäistä kirjainta suurennettaessa"
html_title:           "Go: Merkkijonon ensimmäistä kirjainta suurennettaessa"
simple_title:         "Merkkijonon ensimmäistä kirjainta suurennettaessa"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuttaa merkkijonon ensimmäisen kirjaimen isoiksi kirjaimiksi? Yksi yleinen syy voi olla, että haluat esimerkiksi tulostaa nimen tai lauseen kanssa, ja haluat varmistaa, että se näyttää siistiltä ja hyvin muotoillulta.

## Miten tehdä se

Go-kielessä on sisäänrakennettu funktio nimeltä `strings.Title()`, joka tekee juuri sen haluamasi asian: muuttaa merkkijonon ensimmäisen kirjaimen isoksi kirjaimeksi. Tässä on yksinkertainen esimerkki:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    s := "hei, olen go-ohjelmoija"
    fmt.Println(strings.Title(s))
}
```

Tuloste: "Hei, Olen Go-Ohjelmoija"

Voit myös käyttää `strings.ToUpper()` -funktiota, joka muuttaa kaikki merkit isoiksi kirjaimiksi, ja sitten `strings.ToLower()` -funktiota, joka muuttaa kaikki merkit pieniksi kirjaimiksi. Tässä on esimerkki, jossa ensin muutamme merkkijonon kaikki kirjaimet isoiksi ja sitten takaisin pieniksi:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    s := "Tämä on Merkkijono"
    fmt.Println(strings.ToLower(strings.ToUpper(s)))
}
```

Tuloste: TÄMÄ ON MERKKIJONO

## Syvemmälle

Jos haluat ymmärtää, miten `strings.Title()` -funktio todella toimii, voit tutkia sen lähdekoodia. Voit löytää sen Go-paketin `strings` lähdetiedostosta (tai voit vain tarkastella sitä GitHubissa). Siitä löydät useita muita hyödyllisiä merkkijonojen käsittelyyn liittyviä funktioita.

## Katso myös

- [Go-kielem käsikirja (englanniksi)](https://golang.org/ref/spec)
- [Go-paketin strings lähdetiedosto (englanniksi)](https://github.com/golang/go/blob/master/src/strings/strings.go)