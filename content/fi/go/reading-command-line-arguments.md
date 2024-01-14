---
title:                "Go: Puhekomentoriviparametrien lukeminen"
simple_title:         "Puhekomentoriviparametrien lukeminen"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi lukea komentorivin argumentteja?

Yksi Go-ohjelmoinnin tärkeimmistä taidoista on käyttäjän syöttämän datan käsittely. Komentorivin argumentit ovat yksi tapa saada käyttäjältä tietoa ohjelmalle. Tässä blogikirjoituksessa opit, kuinka lukea ja käsitellä komentorivin argumentteja Go-kielellä.

## Miten tehdä se?

Go-kielellä komentorivin argumentteja voidaan lukea `os`-paketin `Args`-muuttujan avulla. Alla on esimerkki, jolla tulostetaan kaikki komentorivin argumentit:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    args := os.Args[1:]
    for _, arg := range args {
        fmt.Println(arg)
    }
}
```

Jos käytämme yllä olevaa ohjelmaa komentorivillä seuraavasti:

```
go run main.go Hello World
```

Saamme seuraavan tulosteen:

```
Hello
World
```

Kuten näet, `os.Args`-muuttuja tallettaa kaikki komentorivin argumentit `[]string`-tyypin listana. Voit käyttää tavallisia `for`-silmukoita tai `range`-silmukoita käsitelläksesi näitä argumentteja.

## Syvällisempi tarkastelu

Komentorivin argumenttien lukeminen ei rajoitu vain `os.Args`-muuttujan käyttöön. Go-kielessä on myös `flag`-paketti, joka tarjoaa enemmän toiminnallisuutta komentorivin argumenttien käsittelyyn. Voit lukea lisätietoja tästä paketista Go:n [virallisesta dokumentaatiosta](https://golang.org/pkg/flag/).

## Katso myös

- [Go:n viralliset dokumentaatiot komentorivin argumenttien käsittelystä](https://golang.org/pkg/os/#Args)
- [Go:n viralliset dokumentaatiot `flag`-paketista](https://golang.org/pkg/flag/)