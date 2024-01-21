---
title:                "Satunnaislukujen generointi"
date:                  2024-01-20T17:49:04.188626-07:00
model:                 gpt-4-1106-preview
simple_title:         "Satunnaislukujen generointi"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Randomien numeroiden generointi tarkoittaa ennustamattomien numeroiden luomista. Koodarit käyttävät näitä esimerkiksi pelilogiikassa, turvallisuudessa ja simulaatioissa, missä ei voi olla kaavaa tai toistoa.

## How to: (Kuinka tehdä:)
```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// Alusta satunnaislukugeneraattori
	rand.Seed(time.Now().UnixNano())

	// Luo satunnainen kokonaisluku väliltä 0-99
	randomInt := rand.Intn(100)
	fmt.Println(randomInt)

	// Luo satunnainen liukuluku väliltä 0-1
	randomFloat := rand.Float64()
	fmt.Println(randomFloat)
}
```
Sample output:
```
42
0.812428
```
## Deep Dive (Syvä sukellus):
Go:n standardikirjastossa `math/rand` tuotetaan pseudo-satunnaisia lukuja. "Pseudo" tarkoittaa, että ne näyttävät satunnaisilta, mutta ovat todellisuudessa ennalta määrättyjä ja toistettavia. Tämän takia initialisoimme satunnaisuuden `rand.Seed` -funktiolla hetkellisen aikaleiman avulla; se takaa erilaiset tulokset joka suorituskerralla.

Aitoa satunnaisuutta hakevat voivat käyttää `crypto/rand`-pakettia, joka käyttää kryptografisesti turvallisia lähteitä. Go on ottanut esimerkkiä historiallisista PRNGs(estä ("pseudorandom number generator") ja kehittänyt niitä turvallisempaan suuntaan.

## See Also (Katso Myös):
- Go:n virallinen dokumentaatio `math/rand`: https://golang.org/pkg/math/rand/
- Blogipostaus Golangin satunnaisnumeroiden generoinnista: https://blog.golang.org/go-slices-usage-and-internals
- Wiki-sivu pseudo-satunnaistuloksista ja niiden käytöstä eri algoritmeissa: https://en.wikipedia.org/wiki/Pseudorandom_number_generator