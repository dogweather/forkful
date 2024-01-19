---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Gleam: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/concatenating-strings.md"
---

{{< edit_this_page >}}

# Miten merkkijonoja yhdistetään Golang-ohjelmoinnissa

## Mikä & Miksi?
Merkkijonon yhdistäminen on prosessi, jossa liitämme yhteen kaksi tai useampia merkkijonoja. Ohjelmoijat tekevät tämän tehtävän helpottamiseksi ja tehokkuuden lisäämiseksi.

## Näin se tehdään:

```Go
package main
import "fmt"
func main() {
  var str1 = "Hei,"
  var str2 = " maailma!"
  fmt.Println(str1 + str2)
} 
```

Tämä tulostaa:

```
Hei, maailma!
```
## Syventävä tieto:
**Historiallinen konteksti**: Alkuperäissä ohjelmointikielissä, kuten C:ssä, merkkijonojen yhdistäminen oli vaivalloista. Nykypäivän kielissä, kuten Go:ssa, se on paljon helpompaa.

**Vaihtoehdot**: Go tarjoaa myös `strings.Join()` ja `fmt.Sprintf()`, jotka ovat hyödyllisiä pitkien merkkijonojen yhdistämisessä.

```Go
package main
import (
  "strings"
  "fmt"
)
func main() {
  str := []string{"Hei,", " maailma!"}
  fmt.Println(strings.Join(str, ""))
}
```

Tämä tulostaa:

```
Hei, maailma!
```

**Toteutuksen yksityiskohdat**: Go käyttää `+` kertoo kompilaattorille ketjuttaa merkkijonot optimoidusti, joten se on nopein tapa.

## Katso myös:
1. Go:n dokumentaatio merkkijonotoiminnoista: [https://golang.org/pkg/strings/](https://golang.org/pkg/strings/)
2. efficent string concatenation in Go: [https://hermanschaaf.com/efficient-string-concatenation-in-go/](https://hermanschaaf.com/efficient-string-concatenation-in-go/)