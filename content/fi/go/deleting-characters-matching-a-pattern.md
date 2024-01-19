---
title:                "Merkkien poistaminen vastaavalla mallilla"
html_title:           "Arduino: Merkkien poistaminen vastaavalla mallilla"
simple_title:         "Merkkien poistaminen vastaavalla mallilla"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Go Kielen Käyttö Hirviöiden Tuhoamiseen

## Mikä & Miksi?
Hirviöiden tuhoaminen Go-kielissä tarkoittaa tietyt kuviot täyttävien merkkien poistamista. Ohjelmoijat tekevät tämän datan siivoamiseksi tai tietylle kuviolle vastaavien elementtien tuhoamiseksi.

## Kuinka:
Tässä on esimerkki kuinka voit tehdä sen Go:ssa.

```Go
package main 
import (
  "fmt"
  "strings"
)

func main() {
    str := "Hei, maailma!"
    fmt.Println(strings.Trim(str, "!"))
}
```

Se antaa seuraavan tulostuksen:

```
Hei, maailma
```

## Syvällisempi tarkastelu:
Go:n `strings` paketissa on `Trim` funktio, joka poistaa merkkijonon alusta ja lopusta kaikki merkit, jotka vastaavat annettua kuvio. Go:n syntaksi on peräisin C-kielistä, joissa samankaltainen toiminnallisuus saavutetaan `trim` funktiolla.

On olemassa muita vaihtoehtoja merkkien poistamiseen Go:ssa, kuten `TrimLeft`, `TrimRight` tai `TrimSpace`. Valinta riippuu tarpeestasi ja siitä, missä merkkejä haluat poistaa.

Go:n `Trim` on toteutettu vertaamalla merkkijonoa lopusta ja alusta annettuun kuvioon. Se ei suorita regex-kaltaista haku, eli se ei etsi kuviota merkkijonon sisällä, vaan poistaa merkit alusta ja lopusta kunnes se löytää merkin, joka ei kuulu kuvioon.

## Katso Myös:
Go:n dokumentaation `strings` paketti: https://pkg.go.dev/strings

Go's Playground, missä voit kokeilla Go-ohjelmointia selaimessasi: https://play.golang.org