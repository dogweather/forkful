---
title:                "Quotes verwijderen uit een string"
date:                  2024-01-28T22:06:04.064204-07:00
model:                 gpt-4-0125-preview
simple_title:         "Quotes verwijderen uit een string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/removing-quotes-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het verwijderen van aanhalingstekens uit een tekenreeks betekent het wegwerken van die vervelende dubbele of enkele aanhalingstekens die je eigenlijke tekst omhullen. We doen dit om gegevens te saneren, parsingfouten te voorkomen, of tekst voor te bereiden op verdere verwerking zonder de toegevoegde opsmuk van aanhalingstekens.

## Hoe:

Hier is de simpele manier om die aanhalingstekens in Go de deur uit te schoppen:

```go
package main

import (
	"fmt"
	"strings"
)

func removeQuotes(s string) string {
	return strings.Trim(s, "'\"")
}

func main() {
	quotedString := "\"Hallo, Wereld!\""
	fmt.Println("Origineel:", quotedString)

	unquotedString := removeQuotes(quotedString)
	fmt.Println("Zonder aanhalingstekens:", unquotedString)
}
```

De uitvoer ziet er als volgt uit, aanhalingstekens compleet verdwenen:

```
Origineel: "Hallo, Wereld!"
Zonder aanhalingstekens: Hallo, Wereld!
```

## Diepgaand

Vroeger, toen gegevensformaten en -uitwisseling niet gestandaardiseerd waren, konden aanhalingstekens in strings voor chaos zorgen. Dat kunnen ze nog steeds, vooral in JSON of wanneer je strings in databases stopt. Het `strings`-pakket in Go is uitgerust met een `Trim`-functie, die niet alleen witruimte, maar ook elk ander teken waar je geen fan van bent, weghaalt.

Waarom niet Regex? Nou, `Trim` is sneller voor eenvoudige klusjes, maar als je strings verstoppertje spelen met aanhalingstekens op vreemde plaatsen, dan is regex misschien je zware geschut:

```go
import "regexp"

func removeQuotesWithRegex(s string) string {
	re := regexp.MustCompile(`^["']|["']$`)
	return re.ReplaceAllString(s, "")
}
```

Het is als kiezen tussen een schaar en een kettingzaag; kies het gereedschap dat geschikt is voor de klus.

## Zie Ook

Voor meer over het `strings`-pakket en zijn krachtige gereedschappen:
- [Package strings](https://pkg.go.dev/strings)

Om de kracht van reguliere expressies in Go te benutten:
- [Package regexp](https://pkg.go.dev/regexp)

Wil je je verdiepen in de filosofie van het trimmen van strings?
- [De Trim Methode](https://blog.golang.org/strings)
