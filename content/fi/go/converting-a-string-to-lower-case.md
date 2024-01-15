---
title:                "Merkkijonon muuntaminen pienaakkosiksi"
html_title:           "Go: Merkkijonon muuntaminen pienaakkosiksi"
simple_title:         "Merkkijonon muuntaminen pienaakkosiksi"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

## Miksi joku haluaisi muuttaa merkkijonon pieniksi kirjaimiksi?

Merkkijonon muuntaminen pieniksi kirjaimiksi on hyödyllistä esimerkiksi silloin, kun halutaan vertailla kahta merkkijonoa tai kun halutaan varmistaa, että käyttäjän syöttämät tiedot ovat yhtenevät. Pienet kirjaimet ovat myös selkeämpiä ja helpommin luettavissa kuin isot kirjaimet.

## Kuinka tehdä se

Merkkijonon muuntaminen pieniksi kirjaimiksi Go-kielellä on helppoa käyttäen strings.ToLower()-funktiota. Seuraava esimerkki näyttää kuinka muuttaa "HELLO WORLD" pieniksi kirjaimiksi ja tulostaa se konsolille:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    s := "HELLO WORLD"
    lower := strings.ToLower(s)
    fmt.Println(lower)
}
```

Tämä koodi tuottaa seuraavan tulosteen:

```
hello world
```

## Syvempi sukellus

Go-kielessä merkkijonot ovat muutettavissa ja käsiteltävissä käyttäen standardikirjaston strings-pakettia. ToLower()-funktio muuttaa merkkijonon pieniksi kirjaimiksi käyttäen Unicode-käytäntöä, mikä tarkoittaa että se pystyy käsittelemään myös erikois- ja kansainvälisiä merkkejä.

Merkkijonon muuttaminen pieniksi kirjaimiksi on myös mahdollista tehdä käyttäen for-silmukkaa ja rune-tietotyyppiä, joka edustaa yksittäistä Unicode-merkkiä. Tämä antaa enemmän kontrollia muuntamisprosessissa, mutta on myös hieman monimutkaisempi. Alla esimerkki tästä lähestymistavasta:

```Go
package main

import (
    "fmt"
)

func main() {
    s := "Hello, 世界"
    runes := []rune(s)
    for i := 0; i < len(runes); i++ {
        if runes[i] >= 'A' && runes[i] <= 'Z' {
            runes[i] += 32
        }
    }
    fmt.Println(string(runes))
}
```

Tämä tuottaa saman tuloksen kuin aikaisempi esimerkki:

```
hello, 世界
```

## Katso myös

- [Go:n virallinen dokumentaatio strings-paketista](https://golang.org/pkg/strings/)
- [Go by Example - Merkkijonot](https://gobyexample.com/strings)