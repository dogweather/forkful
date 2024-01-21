---
title:                "Merkkien poistaminen hakemalla osumia kaavaan"
date:                  2024-01-20T17:42:15.219177-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkien poistaminen hakemalla osumia kaavaan"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?
Mikä & Miksi?

Poistamme merkkejä vastaamasta kuviota tekstinjalostuksen helpottamiseksi. Se on työkalu, kun puhdistetaan dataa tai muokataan syötteitä haluttuun muotoon.

## How to:
Miten:

Go:ssa käytämme säännöllisiä lausekkeita merkkijonojen karsimiseen. `regexp`-paketti on ystäväsi tässä hommassa. Katso esimerkit:

```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	pattern := "[0-9]"
	str := "Hei 123, täällä Go!"

	re := regexp.MustCompile(pattern)
	processedString := re.ReplaceAllString(str, "")

	fmt.Println(processedString) // Output: "Hei , täällä Go!"
}
```

## Deep Dive
Syväsukellus:

Historiallisesti merkkien poisto kaavoja käyttäen oli työlästä – arvostamme `regexp`-paketin tarjoamaa mukavuutta. Vaihtoehtoina voi kokeilla `strings`-paketin funktioita, mutta ne eivät käsittele kaavoja. Suorituskyvyssä `regexp` voi olla hidas, joten käytä vain tarvittaessa.

## See Also
Katso Myös:

- Go:n virallinen `regexp`-dokumentaatio: [https://pkg.go.dev/regexp](https://pkg.go.dev/regexp)
- `strings`-paketti Go:ssa: [https://pkg.go.dev/strings](https://pkg.go.dev/strings)
- Säännöllisten lausekkeiden opas: [https://www.regular-expressions.info/](https://www.regular-expressions.info/)