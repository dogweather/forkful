---
title:                "Merkkijonojen osien poimiminen"
date:                  2024-01-20T17:45:55.661691-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonojen osien poimiminen"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Substringeilla tarkoitetaan merkkijonoista poimittuja alijonoja. Niitä hyödynnetään, kun halutaan käsitellä vain tiettyjä osia pidemmästä merkkijonosta.

## How to:
```Go
package main

import (
	"fmt"
)

func main() {
	// Alkuperäinen merkkijono
	laulu := "Porilaisten marssi on komeaa kuultavaa"

	// Otetaan substring indekseistä 12 - 19
	substring := laulu[12:20]
	fmt.Println(substring) // Output: "marssi o"

	// Alku ja lopetus rajatta:
	alusta := laulu[:11]  // Porilaisten
	loppu := laulu[21:]  // n komeaa kuultavaa
	fmt.Println(alusta)   // Output: "Porilaisten"
	fmt.Println(loppu)    // Output: "n komeaa kuultavaa"
}
```

## Deep Dive
Go-kielessä on yksinkertainen tapa käsitellä merkkijonoja indeksien avulla, kuten yllä nähtiin. Historiallisesti useimmissa ohjelmointikielissä on jonkinlainen substring-toiminnallisuus, koska se on niin hyödyllinen. Go:n lähestymistapa on mutkattoman tehokas, mutta vaatii ymmärrystä siitä, että indeksointi alkaa nollasta ja viimeinen indeksi on aina n-1.

On olemassa vaihtoehtoisia tapoja, kuten `strings`-kirjaston `Cut` toiminnallisuus, tarjoten hieman erilaisen syntaksin samaan päämäärään:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	laulu := "Porilaisten marssi on komeaa kuultavaa"

	before, after, found := strings.Cut(laulu, "marssi")
	if found {
		fmt.Println(before) // Output: "Porilaisten "
		fmt.Println(after)  // Output: " on komeaa kuultavaa"
	}
}
```

Internally, kun Go leikkaa substringejä, se ei duplikoi dataa vaan antaa uuden viitteen alkuperäisen merkkijonon osaan. Tämä vähentää muistin käyttöä ja nopeuttaa operaatiota.

## See Also
- Go by Example: Strings - https://gobyexample.com/strings
- Go Documentation: Package strings - https://pkg.go.dev/strings
- Go Blog: Strings, bytes, runes and characters in Go - https://blog.golang.org/strings