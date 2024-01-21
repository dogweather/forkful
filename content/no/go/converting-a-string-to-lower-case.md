---
title:                "Konvertere en streng til små bokstaver"
date:                  2024-01-20T17:38:25.899743-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å konvertere en streng til små bokstaver betyr å endre alle bokstavene i strengen til deres minuskelform. Utviklere gjør dette for å forenkle sammenligning av strenger, søk eller for å standardisere data før de lagres.

## Hvordan gjøre det:
```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	original := "Hei, Alle Sammen!"
	lowercased := strings.ToLower(original)
	fmt.Println(lowercased) // Output: hei, alle sammen!
}
```

## Dypdykk
Historisk sett har håndtering av strengekasing vært en utfordring i programmering, spesielt med ulike lokasjoner og språk. Golangs `strings` pakke tilbyr en enkel `ToLower` funksjon som takler de fleste brukstilfeller effektivt. Noen alternativer er å bruke `unicode` pakken for mer spesifikk tilgang eller regulære uttrykk når et komplekst mønster er nødvendig. Implementasjonen av `ToLower` i Go håndterer også unicode og ikke bare ASCII-tegn, noe som er viktig for å støtte internasjonalisering.

## Se Også
- Go docs om `strings` pakke: https://pkg.go.dev/strings
- Go blogg om strenger, bytes, runer og tegn: https://blog.golang.org/strings
- Unicode standard: http://www.unicode.org/standard/standard.html