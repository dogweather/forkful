---
title:    "Go: Generera slumpmässiga nummer"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Varför

Att kunna generera slumpmässiga nummer är en viktig del av många programmeringsuppgifter. Det kan vara användbart för att simulera slumpmässiga händelser, skapa lösenord eller testa algoritmer.

## Hur man gör det

Generering av slumpmässiga nummer i Go är enkelt med hjälp av paketet "math/rand". Här är ett enkelt exempel på att generera ett slumpmässigt heltal mellan 1 och 100:

```
package main

import (
	"fmt"
	"math/rand"
)

func main() {
	randomNum := rand.Intn(100) + 1
	fmt.Println(randomNum)
}
```

Detta kommer att skriva ut ett slumpmässigt heltal mellan 1 och 100 varje gång programmet körs.

För att generera ett slumpmässigt decimaltal mellan 0 och 1 kan du använda funktionen "rand.Float64()".

## Djupdykning

Math/rand-paketet använder en pseudoslumpmässig generator, vilket betyder att det inte är helt slumpmässigt. Det kan dock fortfarande vara användbart för de flesta programmeringsuppgifter. För mer avancerade behov kan man använda paketet "crypto/rand", som använder en kryptografiskt säker generator.

För att se till att de genererade numren inte blir för förutsägbara kan man använda "rand.Seed()" för att justera startvärdet för generatorn.

## Se även

- [Go paket för slumpmässiga nummer](https://golang.org/pkg/math/rand/)
- [Go paket för kryptografiskt säkra slumpmässiga nummer](https://golang.org/pkg/crypto/rand/)
- [Tutorial om slumpmässiga nummer i Go](https://gobyexample.com/random-numbers)