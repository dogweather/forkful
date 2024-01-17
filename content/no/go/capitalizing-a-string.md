---
title:                "Å gjøre en streng stor bokstav"
html_title:           "Go: Å gjøre en streng stor bokstav"
simple_title:         "Å gjøre en streng stor bokstav"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å kapitalisere en streng er å endre alle bokstaver i en streng til store bokstaver. Dette gjøres for å av og til gjøre teksten mer lesbar eller for å følge en bestemt konvensjon i programmeringsspråket.

## Hvordan:
```
Go 'strings' pakken har en innebygd funksjon kalt 'ToUpper' som kan brukes til å kapitalisere en streng.
Eksempel:
Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "hallo verden"
	kapitalisert := strings.ToUpper(str)
	fmt.Println(kapitalisert)
}

// Output: HALLO VERDEN
```

## Dypdykk:
Det å kapitalisere en streng er ikke et nytt konsept, og det er ofte brukt i mange programmeringsspråk. I eldre programmeringsspråk som C ble dette gjort manuelt ved å bruke ASCII-tabellen. I dag finnes det mange andre måter å konvertere strenger til store bokstaver, som for eksempel å bruke regex eller Unicode.

## Se Også:
- https://golang.org/pkg/strings/#ToUpper
- https://en.wikipedia.org/wiki/Capitalization#Computing