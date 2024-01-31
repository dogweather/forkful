---
title:                "De lengte van een string vinden"
date:                  2024-01-28T22:00:09.974341-07:00
model:                 gpt-4-0125-preview
simple_title:         "De lengte van een string vinden"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/finding-the-length-of-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
De lengte van een string bepalen betekent uitzoeken hoeveel karakters deze bevat. Programmeurs doen dit om invoer te valideren, door karakters te loopen, output te limiteren, en meer.

## Hoe:

Om de lengte van een string te krijgen, gebruik `len()`:

```Go
package main

import "fmt"

func main() {
    exampleStr := "Hello, Gophers!"
    length := len(exampleStr)
    fmt.Println(length)  // Uitvoer: 14
}
```

Voor Unicode-karakters of emoji's, is `utf8.RuneCountInString()` je vriend:

```Go
package main

import (
    "fmt"
    "unicode/utf8"
)

func main() {
    exampleStr := "Hello, 世界!"
    length := utf8.RuneCountInString(exampleStr)
    fmt.Println(length)  // Uitvoer: 9
}
```

## Diepere duik
Eenvoudig gezegd, gebruikt Go UTF-8 gecodeerde strings. De ingebouwde `len()`-functie geeft het aantal bytes terug, niet het aantal karakters. Dit is snel maar kan verrassingen opleveren met multi-byte karakters. Voor nauwkeurige karaktertellingen, vooral in wereldwijde toepassingen, gebruik je `utf8.RuneCountInString()` om Unicode correct te verwerken. Historisch gezien telden verschillende talen en bibliotheken karakters op verschillende manieren, maar Unicode is de standaard geworden, en Go's ondersteuning hiervoor is verplicht in het huidige diverse coderingsecosysteem.

Wat betreft alternatieven bieden bibliotheken als `unicode/utf8` robuuste afhandeling van runes, die Unicode-codepunten vertegenwoordigen. Voordat Go de verwerking van Unicode standaardiseerde, moesten programmeurs op maat gemaakte oplossingen implementeren, wat foutgevoelig en complex was.

In implementatiedetails zijn strings in Go onveranderlijke sequenties van bytes. Bij het verwerken van strings dienen programmeurs zich bewust te zijn van mogelijke prestatieverliezen bij het verwerken van erg grote strings of bij overmatig gebruik van `utf8.RuneCountInString()` in prestatie-kritische code, aangezien elke rune moet worden gedecodeerd om nauwkeurig te tellen.

## Zie ook
- The Go Blog over Strings: https://blog.golang.org/strings
- Go `unicode/utf8` pakketdocumentatie: https://golang.org/pkg/unicode/utf8/
- Go `len` functiespecificatie: https://golang.org/ref/spec#Length_and_capacity
