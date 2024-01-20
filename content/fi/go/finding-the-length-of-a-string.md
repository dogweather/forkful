---
title:                "Merkkijonon pituuden selvittäminen"
html_title:           "Go: Merkkijonon pituuden selvittäminen"
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Merkkijonon pituuden selvittäminen Go-ohjelmointikielessä

## Miksi & miksi?
Merkkijonon pituuden selvittäminen on tapa saada selville, kuinka monta merkkiä on määritetyssä merkkijonossa (tekstissä). Ohjelmoijat tekevät tämän usein esimerkiksi datan validointiin tai tulosten käsittelyyn.

## Kuinka:
Tässä on esimerkkikoodi Go-kielellä. Kokeile sitä.

```Go
package main
import "fmt"

func main() {
    str1 := "Ohjelmointi"
    fmt.Println(len(str1))
}
```
Kun suoritat yllä olevan koodin, se tulostaa lukuarvon 12, joka on "Ohjelmointi"-merkkijonon pituus.

## Syvä sukellus
Go-kielessä merkkijonojen pituus lasketaan käyttämällä built-in len()-funktiota. Tämä funktio, joka on ollut alusta alkaen mukana Go:ssa, palauttaa merkkijonon pituuden tavuina, ei merkkeinä, mikä on tärkeää ottaa huomioon, jos käsittelet usean tavun merkkejä.

Vaihtoehtoisesti voit laskea merkkijonon pituuden manuaalisesti käymällä läpi merkkijonon merkit, mutta yleensä len()-funktio on mukavampi ja tehokkaampi tapa.

Merkkijonon pituuden selvittäminen Go:ssa on hyvin tehokasta verrattuna joihinkin muihin ohjelmointikieliin, koska Go tallentaa merkkijonon pituuden merkkijonon alussa, jolloin sen hakeminen on nopeaa.

## Katso myös
1. [Go:n virallinen dokumentaatio Strings-paketista](https://golang.org/pkg/strings/)
2. [Blogikirjoitus merkkijonojen käsittelystä Go:ssa](https://blog.golang.org/strings)