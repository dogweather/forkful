---
title:                "Tekstitiedoston kirjoittaminen"
html_title:           "Go: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Mikä & Miksi?

Kirjoittaminen tekstitiedostoon tarkoittaa sitä, että tallennat tekstiä tietokoneen muistissa olevaan tiedostoon. Ohjelmoijat tekevät tätä, jotta voivat tallentaa ja jakaa tietoa eri ohjelmien ja järjestelmien välillä.

# Kuinka:

Go-kielellä voit helposti avata, kirjoittaa ja tallentaa tiedostoja käyttäen "os" pakettia. Esimerkiksi:

```
package main

import (
    "fmt"
    "os"
)

func main() {
    text := "Tämä on esimerkki tekstistä"
    file, err := os.Create("tekstitiedosto.txt")
    if err != nil {
        fmt.Println(err)
        return
    }
    defer file.Close()
    fmt.Fprintf(file, text)
}
```

Tämä koodi avaa "tekstitiedosto.txt" tiedoston ja tallentaa siihen "Tämä on esimerkki tekstistä" tekstin. Voit myös avata, lukea ja muokata jo olemassa olevia tiedostoja käyttäen "os.Open" ja "bufio" paketteja.

# Syvemmälle:

Tekstin tallentamisen tarve syntyi jo varhaisissa tietokonejärjestelmissä, joissa tiedon jakaminen ja säilyttäminen eri laitteiden välillä oli haasteellista. Nykyään on olemassa myös muita tapoja tallentaa ja jakaa tietoa, kuten tietokantoja ja pilvipalveluita.

# Katso myös:

Voit lukea lisää tekstien käsittelystä Golla Go:n virallisesta dokumentaatiosta: https://golang.org/pkg/os/.