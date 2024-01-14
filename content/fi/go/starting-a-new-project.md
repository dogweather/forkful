---
title:                "Go: Uuden projektin aloittaminen"
simple_title:         "Uuden projektin aloittaminen"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Miksi

Miksi aloittaa uusi projekti Go-ohjelmointikielen avulla? Go on nopea, tehokas ja helposti opittava kieli, joka on suunniteltu skaalautuvuutta silmällä pitäen. Se tarjoaa myös laajan kirjaston erilaisia toimintoja ja työkaluja, jotka tekevät ohjelmoinnista helpompaa ja nopeampaa.

## Kuinka

Go-kieltä oppii parhaiten tekemällä. Tässä on muutama esimerkki siitä, kuinka voit aloittaa uuden projektin ja käyttää Go-kieltä:

```Go
package main

import "fmt"

func main() {
    // Tulostetaan tervehdys konsoliin
    fmt.Println("Hei maailma!")
}
```

Lopputulos:

```
Hei maailma!
```

```Go
package main

import "fmt"

func main() {
    // Luodaan muuttuja
    nimi := "Matti"
    
    // Tulostetaan muuttujan arvo konsoliin
    fmt.Println("Hei", nimi)
}
```

Lopputulos:

```
Hei Matti
```

## Syvemmälle

Aloittaessasi uuden projektin Go-kielellä, muutamia asioita kannattaa pitää mielessä:

- Go on kokonaan käännetty kieli, mikä tarkoittaa, että ohjelmakoodi käännetään suoraan konekielelle. Tämä tekee siitä nopean ja tehokkaan.
- Go-kielen syntaksi on helppo ja selkeä. Se on myös modulaarinen, joten voit helposti jakaa koodiasi eri tiedostoihin ja muokata sitä tarpeidesi mukaan.
- Go:ssa on erittäin laaja vakio-bibliotekki, joka sisältää paljon erilaisia toimintoja ja työkaluja, kuten verkko-ohjelmointiin, tietokantojen hallintaan ja virheiden käsittelyyn liittyviä työkaluja.
- Yhteisö on aktiivinen ja avulias, joten apua on aina saatavilla, jos jokin koodissa mietityttää.

## Katso myös

- [Go:n virallinen sivusto](https://golang.org/)
- [Go:n dokumentaatio](https://golang.org/doc/)
- [Go:n oppimateriaalit](https://golang.org/doc/#learning)
- [Go-yhteisö](https://golang.org/community/)