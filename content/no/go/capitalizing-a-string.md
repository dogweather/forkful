---
title:                "Sette streng til store bokstaver"
html_title:           "Arduino: Sette streng til store bokstaver"
simple_title:         "Sette streng til store bokstaver"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
I Go, å endre en streng til å ha stor bokstav betyr at vi gjør det første tegnet i strengen om til en stor bokstav og lar resten være uendret. Det gjør vi for å følge språklig korrekthet, som i titler, eller for å standardisere tekstdata før lagring og analyse.

## How to:
Go har ingen innebygd metode for å kapitalisere strenger, men du kan bruke `strings` pakken sammen med noen enkle operasjoner. Eksemplet under viser hvordan:

```Go
package main

import (
	"fmt"
	"strings"
	"unicode"
)

func capitalize(s string) string {
	if s == "" {
		return ""
	}
	r := []rune(s)
	return string(unicode.ToUpper(r[0])) + string(r[1:])
}

func main() {
	fmt.Println(capitalize("hei, verden!")) // Output: Hei, verden!
	fmt.Println(capitalize("go er gøy!"))    // Output: Go er gøy!
}
```

## Deep Dive
Å kapitalisere en streng er ikke alltid rett frem i Go fordi språket behandler strenger som en sekvens av bytes, ikke tegn. Det betyr at tegnsett som UTF-8, hvor noen tegn tar mer enn én byte, krever ekstra håndtering. Derfor bruker vi `rune` typen i funksjonen `capitalize` for å støtte multibyte tegn.

Historisk sett, har programmeringsspråk ofte tilbudt string manipulasjonsfunksjoner som en del av standardbiblioteket. Go velger en mer minimalistisk tilnærming, hvor den grunnleggende streng typen er enkel, og ytterligere funksjoner leveres via pakker som `strings` og `unicode`.

En alternativ måte å kapitalisere på ville vært ved bruk av `Title` funksjonen i `strings` pakken, men den konverterer alle ord i strengen, ikke bare det første, så for mange situasjoner er ikke `Title` det du trenger.

Det viktigste å merke seg er at vår `capitalize` metode er enkel og håndterer bare det mest grunnleggende scenariet. I et virkelig use-case scenario kan du møte på strenger med komplekse regler rundt hva som bør være stor bokstav, som i akronymer eller faguttrykk.

## See Also
- Go `strings` pakke: https://pkg.go.dev/strings
- Go `unicode` pakke: https://pkg.go.dev/unicode
- Blogg om streng manipulasjon i Go: https://blog.golang.org/strings
