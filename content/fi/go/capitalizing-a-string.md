---
title:                "Go: Kirjainten muuntaminen isoiksi"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi käyttää Go-kieltä merkkijonon tavujen muuttamisessa

Merkkijonon kirjoittaminen isolla alkukirjaimella voi olla tärkeää monille ohjelmointitehtäville, kuten käyttäjän nimen tietojen tallentamisessa tietokantaan. Go-kielen `strings`-paketissa on kätevä toiminto merkkijonon ensimmäisen tavun muuttamiseksi isolla alkukirjaimella.

##      Miten käytetään `strings`-paketin `Title`-funktiota

 ```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    name := "mari"
    fmt.Println(strings.Title(name))
}
 ```
Output: 
```
Mari
```

## Syvennykää merkkijonon isokirjaimiseen muuttamiseen

`strings.Title`-funktio perustuu `unicode.Title`-funktioon, joka käyttää Unicode-standardia määrittämään merkkien tavujen muuntamisen isoihin ja pieniin kirjaimiin. Se myös huomioi kulttuurisidonnaiset eroavaisuudet eri kielissä.

## Katso myös
- [Go:n virallinen verkkosivusto](https://golang.org/)
- [Go:n dokumentaatio](https://golang.org/pkg/)
- [Tutorial-ohjeet Go-kielestä](https://tour.golang.org/)