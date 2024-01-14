---
title:                "Go: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor
Det å konvertere en streng til små bokstaver i Go kan være nyttig når du for eksempel trenger å sammenligne to strenger uavhengig av store eller små bokstaver. Det kan også gjøre koden din mer lesbar hvis du ønsker å bruke små bokstaver i hele koden.

## Hvordan
For å konvertere en streng til små bokstaver i Go, kan du bruke funksjonen `ToLower` fra `strings` pakken. Her er et eksempel på hvordan du kan gjøre det:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "HEI, JEG ER EN STRENG"
    fmt.Println(strings.ToLower(str))
}
```

Dette vil gi følgende output: `hei, jeg er en streng`.

## Dypdykk
Det kan være nyttig å vite at `ToLower` funksjonen i Go tar hensyn til både ASCII og Unicode bokstaver. Det betyr at den vil konvertere alle store bokstaver til små uansett hvilket språk teksten er skrevet på. Det er også viktig å merke seg at funksjonen ikke vil endre på originalstrengen, men heller returnere en ny streng med de konverterte bokstavene.

## Se også
- [Go dokumentasjon om strings pakken](https://golang.org/pkg/strings/#ToLower)
- [Gode programmeringsprinsipper på norsk](https://www.apress.com/gp/blog/all-blog-posts/all-posts/principles-of-good-programming-by-marian-pierzchala/15162878)
- [Lær Go på norsk](https://github.com/kristianvalind/Norsk-Go)

*Dette blogginnlegget ble skrevet på norsk av en Go-entusiast og er ment å gjøre det enklere for norske lesere å lære og forstå konseptet med å konvertere streng til små bokstaver i Go.*