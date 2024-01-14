---
title:                "Go: Merkkijonon muuntaminen pienen kirjaimen muotoon"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Tekstien muokkaaminen ja käsittely on yleinen osa ohjelmointia, ja usein haluamme muuttaa merkkijonon kirjainkoko kirjoitustyylin tai tietokannan vaatimusten mukaiseksi. Tässä blogikirjoituksessa käsittelemme, kuinka muunnetaan merkkijono pienaakkosiksi Go-ohjelmointikielen avulla.

## Kuinka tehdä se

Go-kielen sisäänrakennetulla `strings`-paketilla on kätevä toiminto, `ToLower`, jonka avulla voidaan muuttaa merkkijono pienaakkosiksi. Katso alla oleva koodiesimerkki ja sen tuloste.

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    s := "Tämä on Esimerkki MERKKIJONOsta"
    fmt.Println(strings.ToLower(s))
}
```
Tuloste: `tämä on esimerkki merkkijonosta`

## Syvällinen sukellus

Go-kielen `strings`-paketin `ToLower`-funktio käyttää Unicode-alustan alustasääntöjä. Se käsittelee merkkijonot jokaisen kirjaimen nimellisen käyvän arvon perusteella, jolloin se on sovellettavissa moniin eri kieliin. Lisäksi `ToLower`-funktio on myös kulttuuriherkkä, mikä tarkoittaa, että se ottaa huomioon kulttuurin ja maan erityispiirteet muunnettaessa merkkijonon pienaakkosiksi.

## Katso myös

- [Go-kielen strings-paketti dokumentaatio](https://golang.org/pkg/strings/)
- [Harjoittele merkkijonojen muokkaamista Go-kurssilla](https://www.golang-book.com/books/intro/8)
- [Tietokannan vaatimusten mukaisen merkkijonon muunnos Go-kielen avulla](https://blog.arkency.com/how-to-efficiently-transform-a-string-to-lower-case-in-go/)