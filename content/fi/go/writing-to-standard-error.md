---
title:                "Go: Standardi virhekirjoitusten tekeminen"
simple_title:         "Standardi virhekirjoitusten tekeminen"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kirjoittaa standard error -virtaan? Go-kielessä on monia tapoja käsitellä virheitä ja tulostaa tietoa ohjelman suorituksen aikana. Kuitenkin, jos haluat tulostaa viestin, joka näkyy vain silloin kun virhe tapahtuu, standard error on oikea valinta.

## Kuinka tehdä

Koodi näyttää, kuinka kirjoittaa viesti standard error -virtaan käyttäen Go-kielestä löytyvää "fmt" -pakettia:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Tulostetaan yksinkertainen viesti standard error -virtaan
    fmt.Fprintln(os.Stderr, "Tämä on viesti standard error -virtaan")
}
```

Tulostus:

```
Tämä on viesti standard error -virtaan
```

## Syvemmälle

Standard error -virta on tarkoitettu virheilmoitusten ja varoitusten tulostamiseen ohjelman suorituksen aikana. Tämä erillinen virta mahdollistaa erottelun tavallisesta tulostuksesta ja helpottaa virheiden jäljittämistä ja korjaamista.

## Katso myös

- [Go:n virallinen opas virheiden käsittelyyn](https://blog.golang.org/error-handling-and-go)
- [fmt-paketin dokumentaatio](https://golang.org/pkg/fmt/)
- [os-paketin dokumentaatio](https://golang.org/pkg/os/)