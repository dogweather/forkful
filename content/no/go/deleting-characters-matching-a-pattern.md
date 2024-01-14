---
title:                "Go: Slette tegn som samsvarer med mønster"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Å slette karakterer som matcher et mønster kan være en viktig del av å skrive effektive og strukturerte programmer. Dette kan gjøre det enklere å filtrere og behandle data i programmet ditt.

## Hvordan

For å slette karakterer som matcher et mønster i Go, kan du bruke "strings.ReplaceAll()" funksjonen. Denne funksjonen tar inn tre parametere: den originale teksten, mønsteret du vil slette og tekststrengen du vil erstatte den med.

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    originalTekst := "Jeg vil slette disse karakterene: @#$%&^"
    slettMønster := "@#$%&^"
    erstattTekst := ""

    slettetTekst := strings.ReplaceAll(originalTekst, slettMønster, erstattTekst)
    fmt.Println(slettetTekst)
}

// Output:
// Jeg vil slette disse karakterene:
```

Som du kan se i eksempelet over, blir alle karakterene i mønsteret slettet og erstattet med en tom streng. Du kan også bruke denne funksjonen til å erstatte mønsteret med en annen tekststreng om ønskelig.

## Dypdykk

Det finnes også andre måter å slette karakterer som matcher et mønster i Go. Du kan for eksempel bruke regulære uttrykk ved hjelp av "regexp" pakken. Dette gir deg mer kontroll over mønsteret du vil slette, og du kan også implementere mer avanserte slettemønstre.

## Se også

- [Go Strings](https://golang.org/pkg/strings/)
- [Regular Expressions in Go](https://golang.org/pkg/regexp/)