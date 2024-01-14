---
title:    "Go: Läsning av kommandoradsargument"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Om du någonsin har arbetat med cmd eller terminaler, så har du förmodligen stött på begreppet "command line arguments" eller "kommandoradsparametrar". Dessa är viktiga för att kunna köra program på ett flexibelt sätt. I denna bloggpost kommer vi att dyka djupare in i hur man läser in dessa argument i Go programmeringsspråket.

## Så här gör du

För att läsa in kommandoradsparametrar i Go, behöver vi använda os.Args variabeln. Denna variabel innehåller en array av strängar, där varje element är ett kommandoradsargument. Låt oss titta på ett enkelt exempel:

```Go
package main 

import (
    "fmt"
    "os"
)

func main() {
    // Läser in första argumentet efter programnamnet
    arg1 := os.Args[1] 
    // Skriver ut argumentet till terminalen
    fmt.Println("Argument 1:", arg1)
}
```

För att köra detta program med några argument, skriv detta i din cmd eller terminal:

```
go run main.go argument1
```

Outputen bör se ut som följande:

```
Argument 1: argument1
```

Vi använde os.Args[1] eftersom det första elementet i arrayen är programnamnet, i detta fall "main.go". Om vi skulle vilja läsa in flera argument, kan vi använda en loop för att läsa igenom hela os.Args arrayen.

```Go
for i := 2; i < len(os.Args); i++ {
    // Läser in varje argument och skriver ut dem
    arg := os.Args[i]
    fmt.Println("Argument", i, ":", arg)
}
```

## Djupdykning

Nu när vi vet hur man läser in kommandoradsparametrar i Go, låt oss titta på några mer avancerade saker vi kan göra med dem. Vi kan till exempel använda flags från "flag" paketet för att ge våra parametrar värden eller använda olika flaggor för olika funktionalitet. Det finns också möjlighet att läsa in argument som numeriska värden eller booleans.

Det är också viktigt att notera att argument som innehåller mellanslag behöver omges med citattecken när vi kör programmet. Annars kommer dessa argument att tolkas som separata argument. Detta kan åstadkommas med hjälp av "strings" paketet och funktionen "Join".

## Se även

- [The Go Programming Language - Command Line Arguments](https://golang.org/doc/go1.14#os)
- [Go by Example - Command-Line Arguments](https://gobyexample.com/command-line-arguments)
- [arg - Flag Parsing Library for Go](https://github.com/alexflint/arg)