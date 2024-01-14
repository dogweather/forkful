---
title:    "Go: Komentoriviparametrien lukeminen"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Miksi

Kun kehität ohjelmistoa Go-kielellä, on tärkeää ymmärtää, miten lukea komentoriviargumentteja. Tämä on hyödyllistä, kun haluat antaa käyttäjän antaa ohjelmalle erilaisia toimintoja ja vaihtoehtoja. Jatka lukemista ja opit, miten voit lukea komentoriviargumentteja Go-kielellä.

## Kuinka

Lue komentoriviargumentteja käyttämällä `os.Args` -muuttujaa, joka sisältää kaikki annetut argumentit taulukossa. Voit tulostaa kaikki argumentit käyttämällä `fmt.Println(os.Args)`. Tässä on esimerkki koodista:

```Go
package main

import (
  "fmt"
  "os"
)

func main() {
  fmt.Println(os.Args)
}
```

Kun suoritat tämän koodin komentorivillä ja annat sille argumentteja, näet tulosteessa kaikki annetut argumentit.

```
go run main.go arg1 arg2 arg3
```

Tulostus olisi seuraavanlainen:

```
[arg1 arg2 arg3]
```

Voit myös käyttää `len()` -funktiota tarkistaaksesi, kuinka monta argumenttia on annettu. Esimerkiksi `len(os.Args)` palauttaisi 4, koska `os.Args` sisältää myös ohjelman nimen.

## Syventävä tieto

`os.Args` sisältää myös muita tietoja, kuten tiedostopolun ja argumenttien indeksinumeron. Voit käyttää tätä tietoa lisätoimintojen toteuttamiseen. Voit myös käyttää pakettia `flag` helpottamaan komentoriviargumenttien käsittelyä.

## Katso myös

- [Go-kielen virallinen dokumentaatio](https://golang.org/doc/)
- [Go-kielen komentoriviargumentteihin liittyvät ohjeet](https://golangdocs.com/command-line-arguments-in-golang)