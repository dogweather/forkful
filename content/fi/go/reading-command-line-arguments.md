---
title:                "Komentoriviparametrien lukeminen"
html_title:           "Go: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

On monia tilanteita, joissa tarvitset käyttäjän syötettä tai parametreja suorittaaksesi ohjelman tietyllä tavalla. Esimerkiksi komentorivillä suorittaessa on kätevää antaa ohjelmalle tietoa ennen sen suorittamista. Siksi on hyödyllistä tietää, miten komentoriviparametreja voidaan lukea ja käsitellä Go-kielellä. 

## Miten tämä tehdään

Komentoriviparametrit on helppo lukea Go-kielellä, sillä siihen sisältyy valmiita toimintoja niiden käsittelyyn. Voit lukea parametreja käyttämällä `os.Args` -muuttujaa, joka palauttaa kaikki parametrit taulukkona. Voit käyttää myös `flag` -pakettia, joka tarjoaa mahdollisuuden määritellä ja lukea tiettyjä parametreja.

```Go
package main

import (
    "flag"
    "fmt"
    "os"
)

func main() {
    // Lukee komentoriviparametrit ja tallentaa ne taulukkoon
    args := os.Args
    
    // Lukee parametrit f-lippuun
    var fileName string
    flag.StringVar(&fileName, "f", "", "Tiedoston nimi")
    flag.Parse()
    
    fmt.Println("Kaikki parametrit:", args)
    fmt.Println("Tiedoston nimi:", fileName)
}
```

### Tulostus

```
$ go run main.go -f testi.txt

Kaikki parametrit: [main.go -f testi.txt]
Tiedoston nimi: testi.txt
```

## Syvällistä

Parametrit luetaan ohjelman argumenteista, jotka annetaan sen suorituksen yhteydessä. Ne voivat sisältää tietoa mm. halutusta tiedostosta, suoritusasetuksista tai muista tarpeellisista tiedoista. Komentoriviparametrit ovat käyttäjälle helposti muokattavissa ja ohjaavat ohjelman suoritusta halutulla tavalla. 

## Katso myös

- [Go-kielen virallinen dokumentaatio komentoriviparametrien lukemisesta](https://golang.org/pkg/os/)
- [Esimerkkejä käytöstä ja selityksiä Go-kielen `flag`-paketista](https://gobyexample.com/command-line-flags)