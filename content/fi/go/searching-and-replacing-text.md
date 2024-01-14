---
title:    "Go: Tekstin etsiminen ja korvaaminen"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Miksi

Miksi haluat käyttää tekstiin haku- ja korvaustoimintoa? Se on hyödyllinen tekniikka tekstin muokkaamiseen nopeasti, erityisesti silloin kun työskentelet suurten tekstimäärien kanssa. Se voi säästää aikaa ja vaivaa manuaalisen muokkaamisen sijaan.

## Kuinka

Voimme käyttää Go-ohjelmointikielen ```strings``` kirjastoa etsimään ja korvaamaan tekstiä. Alla on esimerkki koodista, joka korvaa kaikki ```hello``` sanat ```hej``` sanoilla:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	text := "Hello world, hello everyone!"
	replacedText := strings.ReplaceAll(text, "hello", "hej")

	fmt.Println(replacedText)

	// Output: Hej world, hej everyone!
}
```

## Syvempi sukellus

Etsimisen ja korvaamisen lisäksi Go:ssa on myös muita työkaluja tekstinkäsittelyyn, kuten ```regexp``` kirjasto, joka mahdollistaa monimutkaisempien sääntöjen käytön tekstiä etsiessä ja korvatessa. Go sisältää myös useita muita hyödyllisiä kirjastoja, jotka voivat auttaa tekstinkäsittelyssä.

## Katso myös

- [Go-kielen viralliset dokumentit](https://golang.org/doc/)
- [Go-kielen ```strings``` kirjaston dokumentit](https://golang.org/pkg/strings/)
- [Go-kielen ```regexp``` kirjaston dokumentit](https://golang.org/pkg/regexp/)
- [Go-kielen muita tekstinkäsittelyyn liittyviä kirjastoja](https://github.com/avelino/awesome-go#text-processing)