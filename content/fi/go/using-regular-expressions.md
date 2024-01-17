---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Go: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Mikä ja Miksi?

Säännöllisten lausekkeiden käyttö on tapa etsiä ja korvata tekstissä esiintyviä merkkijonoja tiettyjen sääntöjen mukaan. Ohjelmoijat käyttävät tätä työkalua parantaakseen koodin tehokkuutta ja tarkkuutta.

## Kuinka tehdä:

Koodiesimerkit ja näytetulosteet:

```Go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    // Luodaan säännöllinen lauseke, joka etsii kaikki sanat, jotka alkavat kirjaimella "G"
    pattern := regexp.MustCompile(`\bG\w+`)
    phrase := "Go programming is great!"
    // Etsitään lausekkeen avulla sana, joka alkaa kirjaimella "G" ja tulostetaan se
    result := pattern.FindString(phrase)
    fmt.Println(result)
    // => "Go programming"
}
```

## Syväluotaus:

Säännölliset lausekkeet ovat olleet olemassa jo vuosikymmeniä ja ne ovat edelleen suosittu työkalu ohjelmoinnissa. On myös vaihtoehtoja, kuten positiivisen ja negatiivisen katkelman tunnistimet, mutta säännölliset lausekkeet tarjoavat laajemman valikoiman ominaisuuksia.

Go-kielessä säännölliset lausekkeet on toteutettu käyttäen pakettia "regexp", joka tarjoaa helpon ja tehokkaan tavan käsitellä tekstiä. Lausekkeet voivat sisältää erilaisia ​​operaattoreita ja metakaraktereita, jotka tekevät niistä erittäin monipuolisia työkaluja.

## Katso myös:

- "regexp" -pakettidokumentaatio: https://golang.org/pkg/regexp/
- Säännöllisten lausekkeiden opas: https://www.regular-expressions.info/