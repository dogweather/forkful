---
title:                "Tarkistetaan, onko hakemistoa olemassa"
html_title:           "Go: Tarkistetaan, onko hakemistoa olemassa"
simple_title:         "Tarkistetaan, onko hakemistoa olemassa"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

On olemassa monia tilanteita, joissa voit joutua tarkistamaan, onko tietyssä hakemistossa olemassa tiedostoja. Tämä voi olla hyödyllistä esimerkiksi tietoturvallisuuden kannalta, jotta varmistat, etteivät arkaluonteiset tiedostot päädy vääriin käsiin.

## Kuinka

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Tarkistetaan, onko hakemisto "tiedostot" olemassa
    _, err := os.Stat("tiedostot")
     
    // Jos err muuttuja on nil, hakemisto on olemassa
    if err == nil {
        fmt.Println("Hakemisto löytyi!")
    } else {
        // Muussa tapauksessa tulostetaan virheilmoitus
        fmt.Println("Virhe:", err)
    }
}
```

Esimerkissä käytetään `os.Stat()` -funktiota, joka palauttaa nil-arvon, jos hakemisto on olemassa, ja muussa tapauksessa virheen.

## Syvemmälle

`os.Stat()` -funktio käyttää käyttöjärjestelmän `stat()` -systeemikutsua tarkistaakseen, onko tiedosto tai hakemisto olemassa. Tämä kutsu palauttaa erilaisia tietoja tiedoston tai hakemiston tilasta, kuten koko, aikaleimat ja käyttöoikeudet. Näitä tietoja voidaan käyttää tarkempiin tarkastuksiin.

## Katso myös

- [Go'n virallinen dokumentaatio os-paketeista](https://golang.org/pkg/os/)
- [Go'n dokumentaatio os-paketin Stat()-funktiosta](https://golang.org/pkg/os/#Stat)
- [Stack Overflow -keskustelu tiedoston tai hakemiston olemassaolon tarkistamisesta Go:lla](https://stackoverflow.com/questions/12518876/how-to-check-if-a-file-exists-in-go)