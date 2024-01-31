---
title:                "Merkkijonon pituuden selvittäminen"
date:                  2024-01-20T17:47:45.231792-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon pituuden selvittäminen"

category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Mittaus on avainymmärrys. Ohjelmoinnissa merkkijonon pituuden selvittäminen kertoo meille, kuinka monta merkkiä jono sisältää. Tällä tiedolla säädämme logiikkaamme - oli kyseessä sitten datan validointi, käyttöliittymän asettelut, tai tekstin käsittely.

## How to:
Go:n standardikirjasto tekee merkkijonon pituuden selvittämisen helpoksi. Käytä `len`-funktiota:

```Go
package main

import (
	"fmt"
)

func main() {
	str := "moikka"
	fmt.Println("The length of the string is:", len(str))
}
```

Tulostus:

```
The length of the string is: 6
```

## Deep Dive
Merkkijonon pituuden selvittäminen ei ole aina suoraan verrannollista merkin määrään. Historiallisesti merkkijonojen pituuden laskeminen on ollut yksinkertaisempaa, kun käytössä oli vain ASCII-merkistö. Go:ssa merkkijonot on koodattu UTF-8-muodossa, joka mahdollistaa monien eri kirjoitusjärjestelmien koodaamisen. UTF-8 merkistökoodauksessa merkki voi viedä 1–4 tavua.

Jos haluat selvittää Unicode-koodipisteiden määrän (todellisen "merkkien" määrän), sinun tulee käyttää `utf8`-pakettia:

```Go
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	str := "hei"
	fmt.Println("Number of bytes:", len(str))
	fmt.Println("Number of runes:", utf8.RuneCountInString(str))
}
```

Tämä on tärkeää, kun työstät Unicode-merkkejä sisältäviä merkkijonoja, kuten "你好", "Hello", tai "مرحبا".

## See Also
Ohjelmoinnin sävyttämä ymmärrys kasvaa syventymällä. Alla muutamia hyödyllisiä lähteitä:

- Go:n viralliset dokumentit merkkijonoista: https://golang.org/pkg/strings/
- Unicode standardi: http://www.unicode.org/standard/standard.html
- UTF-8 merkistökoodauksesta Wikipediassa: https://fi.wikipedia.org/wiki/UTF-8
