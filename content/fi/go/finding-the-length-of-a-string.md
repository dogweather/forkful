---
title:                "Go: Lausekkeen pituuden löytäminen"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi
Halutessaan selvittää merkkijonon pituuden, on hyödyllistä ymmärtää miten tämä toimii ja miten se voi auttaa ohjelmoinnissa.

## Miten
```Go
package main
import "fmt"
func main() {
  s := "Moi maailma!"
  fmt.Println(len(s))
}
```
Tämä yksinkertainen koodiesimerkki näyttää kuinka voit käyttää len() funktiota Go-kielellä selvittääksesi merkkijonon pituuden. Koodin suoritus tuottaa seuraavan tulosteen: `12`.

## Syvempi sukellus
Merkkijonojen pituuden löytäminen on tärkeä osa ohjelmointia, sillä se voi auttaa käsittelemään dataa tehokkaasti ja vähentämään virheitä. Go-kielellä, merkkijonojen pituuden löytämiseksi voit käyttää len() funktiota, joka palauttaa merkkijonon merkkien määrän. Toisin sanoen, se palauttaa numeron esittäen merkkien määrän, mukaan lukien välilyönnit ja erikoismerkit.

## Katso myös
- [Golang - Merkkijonojen käsittely](https://github.com/golang/example/tree/master/stringutil)
- [Golang len() funktio dokumentaatio](https://golang.org/pkg/builtin/#len)