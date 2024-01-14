---
title:    "Go: Konvertering av en streng til små bokstaver"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en streng til små bokstaver er en vanlig oppgave i programmering, spesielt når man jobber med tekstbehandling. Ved å gjøre dette, kan man gjøre søk og sammenligninger av tekst mer nøyaktig og effektivt. 

## Hvordan

```Go
package main

import (
	"fmt"
	"strings"
)

func makeLowerCase(str string) string {
	return strings.ToLower(str)
}

func main() {
	str := "HELLO, WORLD!"
	fmt.Println(makeLowerCase(str))
}
```

Output:

```
hello, world!
```

## Dypdykk

Å konvertere en streng til små bokstaver kan virke enkelt, men det er viktig å være oppmerksom på ulike faktorer som kan påvirke resultatet. For eksempel kan noen bokstaver i andre språkskrifter ha spesielle regler for små bokstaver. I Go er det også mulig å bruke ulike funksjoner og metoder for å tilpasse konverteringen etter behov.

## Se også

* [strings.ToLower() dokumentasjon](https://golang.org/pkg/strings/#ToLower)
* [Slik bruker du strings.ToLower i Go](https://www.calhoun.io/using-the-basics-of-the-strings-package-in-go/)

Takk for at du leste! Har du spørsmål eller tilbakemeldinger, ikke nøl med å kontakte meg. Lykke til med konvertering av strenger til små bokstaver i dine fremtidige Go-prosjekter!