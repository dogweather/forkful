---
title:    "Go: Generera slumpmässiga tal"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Alla programmerare stöter på situationer där de behöver slumpmässiga tal för att lösa problem eller skapa spel. Det är här som funktionen för att generera slumpmässiga nummer kommer väl till hands. I denna bloggpost kommer vi att utforska hur man skapar slumpmässiga tal i Go-programmeringsspråket.

## Hur man gör det

För att generera slumpmässiga tal i Go kan vi använda funktionen "Intn()" från paketet "rand". Detta paket innehåller funktioner för att skapa olika typer av slumpmässiga tal, som heltal, flyttal och booleska värden.

```Go
package main

import (
    "fmt"
    "math/rand"
)

func main() {
    // Generera ett slumpmässigt heltal mellan 0 och 10
    randomInt := rand.Intn(11)
    fmt.Println("Slumptal:", randomInt)

    // Generera ett slumpmässigt flyttal mellan 0 och 1
    randomFloat := rand.Float64()
    fmt.Println("Slumpmässigt flyttal:", randomFloat)

    // Generera ett slumpmässigt booleskt värde
    randomBool := rand.Bool()
    fmt.Println("Slumpmässigt booleskt värde:", randomBool)
}
```
Output:
```
Slumptal: 7
Slumpmässigt flyttal: 0.17858786857274656
Slumpmässigt booleskt värde: true
```

## Djupdykning

Go använder sig av ett pseudoslumpmässigt talgenerator, vilket betyder att generatorn följer ett förutbestämt mönster och inte genererar helt slumpmässiga tal. Detta mönster bestäms av en startpunkt, som kallas för "seed". För att garantera olika sekvenser av slumpmässiga tal vid varje körning, är det viktigt att ändra seed-värdet.

För att ändra seed-värdet kan vi använda funktionen "Seed()" från paketet "rand". Vi kan till exempel använda ett tidsbaserat värde som seed för att få ett annorlunda resultat vid varje körning.

## Se även

- [Go-paketet "rand" dokumentation](https://golang.org/pkg/math/rand/)
- [Dokumentation för funktionen "Intn()"](https://golang.org/pkg/math/rand/#Intn)
- [Dokumentation för funktionen "Seed()"](https://golang.org/pkg/math/rand/#Seed)