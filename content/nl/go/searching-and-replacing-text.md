---
title:                "Tekst zoeken en vervangen"
date:                  2024-01-28T22:07:13.504075-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tekst zoeken en vervangen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/searching-and-replacing-text.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Zoeken en vervangen van tekst betreft het vinden van specifieke reeksen van tekens in een string en deze verwisselen met verschillende tekens. Programmeurs doen dit voor alles van het corrigeren van typfouten in enorme datasets tot het automatiseren van code-refactoring in veel bestanden.

## Hoe te:

Go's standaardbibliotheek `strings` heeft wat je nodig hebt. Hier is hoe je `strings.Replace` gebruikt:

```go
package main

import (
	"fmt"
	"strings"
)

func main() {
	replacedString := strings.Replace("Hello, Go!", "Go", "World", -1)
	fmt.Println(replacedString) // Uitvoer: Hello, World!
}
```

`-1` betekent alle instanties vervangen. Om alleen de eerste instantie te vervangen, gebruik in plaats daarvan `1`.

Als je complexere vervangingen wilt doen waarbij patronen betrokken zijn, zul je waarschijnlijk `regexp` gebruiken:

```go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	regex := regexp.MustCompile(`(Go)`)
	replacedString := regex.ReplaceAllString("Hello, Go! Go is geweldig.", "Gopher")
	fmt.Println(replacedString) // Uitvoer: Hello, Gopher! Gopher is geweldig.
}
```

Regex is krachtig, maar gebruik het niet te veel. Voor eenvoudige dingen, gebruik `strings`.

## Diepere Duik

Go was niet de eerste taal die tekstvervanging deed, maar zijn standaardbibliotheek is gebruiksvriendelijk. Unix-tools zoals `sed` gingen al lang voor Go om met zoek-en-vervang, met behulp van reguliere expressies. Go's `regexp` pakket geeft die kracht programmatisch.

In vergelijking met andere talen, wisselt Go een beetje van ruwe snelheid voor veiligheid en leesbaarheid. Andere hulpmiddelen en talen kunnen sneller zijn voor tekstverwerking (zoals Perl), maar Go's balans tussen gebruiksgemak en prestaties is een sterk punt.

Wanneer je zoekt-en-vervangt in Go doet, onthoud:
- `strings` voor eenvoudige zaken.
- `regexp` voor patronen.
- Het laatste argument in `strings.Replace` bepaalt het aantal vervangingen.

## Zie Ook

- Go by Example: Stringfuncties - https://gobyexample.com/strings
- Go by Example: Reguliere Expressies - https://gobyexample.com/regular-expressions
- Go Pakket strings - https://pkg.go.dev/strings
- Go Pakket regexp - https://pkg.go.dev/regexp
