---
title:                "Go: Komentoriviparametrien lukeminen"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi
Komentoriviparametrien lukeminen on tärkeä taito jokaiselle Go-ohjelmoijalle. Käyttämällä tätä toimintoa voit antaa ohjelmillesi argumentteja suoraan komentoriviltä, mikä parantaa niiden joustavuutta ja käytettävyyttä.

## Kuinka
Komennot voidaan lukea Go-kielellä käyttämällä `os` -pakettia ja sen `Args` -muuttujaa. Tämä muuttuja sisältää kaikki komentoriviltä saadut argumentit, joten voit käsitellä niitä ohjelmassasi haluamallasi tavalla. Alla on esimerkkikoodi, joka tulostaa komentoriviltä saadut argumentit:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    arguments := os.Args

    fmt.Println("Komentoriviltä saadut argumentit:")
    for i := 1; i < len(arguments); i++ {
        fmt.Println(arguments[i])
    }
}
```
Esimerkkitulostus:
```
Komentoriviltä saadut argumentit:
Hello
World
```

## Syvällinen sukellus
Komentoriviparametrien lukeminen ei rajoitu vain yksinkertaiseen tulostamiseen. Voit myös käsitellä niitä monipuolisesti ja hyödyntää niitä ohjelman logiikassa. Esimerkiksi voit käyttää tiettyjä argumentteja asetusmuuttujina tai vertailla niitä tiettyihin parametreihin. Voit myös validoida argumentteja ja hälyttää käyttäjälle, jos annetut arvot eivät ole kelvollisia.

## Katso myös
- [os-paketti Go-kielen virallisessa dokumentaatiossa](https://golang.org/pkg/os/)
- [Go-ohjelmointikielen perusteet](https://www.golang-book.com/books/intro)