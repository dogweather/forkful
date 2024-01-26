---
title:                "Merkkijonon interpolointi"
date:                  2024-01-20T17:50:56.537045-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon interpolointi"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä & Miksi?)
Interpoloimalla merkkijonoa yhdistetään muuttujan arvot ja teksti. Koodarit tekevät sen tehdäkseen viesteistä dynaamisia ja ylläpitääkseen koodin selkeyttä.

## How to: (Kuinka tehdään:)
```go
// Esimerkki merkkijonon interpoloinnista Go:ssa

package main

import (
	"fmt"
)

func main() {
	name := "Maarit"
	age := 28
	greeting := fmt.Sprintf("Hei, nimeni on %s ja olen %d vuotta vanha.", name, age)
	
	fmt.Println(greeting)
	// Tämä tulostaa: Hei, nimeni on Maarit ja olen 28 vuotta vanha.
}
```

## Deep Dive (Syväsukellus)
Ennen kuin Go tuli kuvioihin, kielet kuten Perl ja PHP käyttivät merkkijonon interpolointia laajasti. Go:ssa merkkijonon interpolointi tehdään `fmt.Sprintf` funktiolla. 

Vaihtoehtona voit käyttää plusmerkkiä (`+`) yhdistääksesi merkkijonot, mutta se ei ole yhtä joustavaa tai tehokasta.

`fmt.Sprintf` antaa interpoloida tyylikkäästi eri tietotyyppejä käyttämällä muotoilumerkkijonoja, kuten `%s` merkkijonoille ja `%d` kokonaisluvuille. Tämä toiminto muuttaa tiedon dynaamisesti osaksi merkkijonoa ja palauttaa uuden merkkijonon ilman alkuperäisen muuttujan muokkaamista.

## See Also (Katso Myös)
- Go:n fmt-paketti: https://pkg.go.dev/fmt
- Verkkokurssi merkkijonon käsittelystä Go:ssa: https://www.coursera.org/learn/golang-getting-started
- Go:n dokumentaatio: https://golang.org/doc/
