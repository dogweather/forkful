---
title:                "Searching and replacing tekst"
html_title:           "Go: Searching and replacing tekst"
simple_title:         "Searching and replacing tekst"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor ville noen engasjere seg i å søke og erstatte tekst? Det kan være nyttig når man ønsker å gjøre endringer i en større kodebase eller når man ønsker å standardisere tekstformatet i et dokument.

## Hvordan

Du kan bruke `strings.Replace` funksjonen i Go for å søke og erstatte tekst. Her er et eksempel på hvordan du bruker den:

```Go
package main

import (
  "fmt"
  "strings"
)

func main() {
  str := "Hei, verden!"
  // Erstatter "Hei" med "Hallo" i teksten
  result := strings.Replace(str, "Hei", "Hallo", 1)
  fmt.Println(result) // Hallo, verden!
}
```

Her erstatter vi den første forekomsten av "Hei" med "Hallo" i teksten. `1` parameteret sier til funksjonen å bare gjøre en enkelt utskifting. Du kan også bruke `-1` for å erstatte alle forekomstene av tekst.

## Dykk Dypere

`strings.Replace` funksjonen tar også inn et `count` parameter som bestemmer hvor mange ganger teksten skal erstattes. Som standard, erstattes alle forekomstene. I tillegg kan man også bruke `strings.ReplaceAll` som forenkler koden ved å automatisk sette `count` til `-1`.

## Se også

- [Go strings pakken](https://pkg.go.dev/strings)
- [Andre nyttige string funksjoner i Go](https://yourbasic.org/golang/string-functions-reference-cheat-sheet/)