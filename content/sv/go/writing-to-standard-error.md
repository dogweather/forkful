---
title:                "Skrivning till standardfel"
html_title:           "Go: Skrivning till standardfel"
simple_title:         "Skrivning till standardfel"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva till standardfel (standard error) innebär att skicka ut felmeddelanden och andra typer av output till ett speciellt strömutflöde istället för det vanliga utflödet som används för att visa resultat. Detta är användbart för att separera olika typer av information och göra det lättare att felsöka och hitta eventuella fel i koden.

## Hur:
I Go kan du skriva till standardfel genom att använda funktionen ```fmt.Fprintf(os.Stderr, format, args...)```. Det är viktigt att notera att alla argument efter formatet i denna funktion måste vara av typen ```interface{}```, vilket innebär att det kan vara vilken typ som helst.

Exempelkod:
```
package main

import (
    "fmt"
    "os"
)

func main() {
    // Variabel för att simulera ett felmeddelande
    err := fmt.Errorf("Något gick fel")
    
    // Skriv ut till standardfel
    fmt.Fprintf(os.Stderr, "Ett fel uppstod: %v", err)
}
```

Exempeloutput (till standardfel):
```
Ett fel uppstod: Något gick fel
```

## Djupdykning:
Att skriva till standardfel är vanligt i många programmeringsspråk och är en viktig del av felhantering och felsökning. Det ger möjlighet att skilja på olika typer av information och hantera dem på olika sätt. Alternativet till att skriva till standardfel är att skriva till standardutflödet, vilket är det vanliga sättet att visa resultat på. Men om koden innehåller mycket output eller felmeddelanden kan det bli svårt att hitta eventuella fel i resultatet.

I Go är standardfel representerat av variabeln ```os.Stderr```, vilket är ett ```os.File``` objekt som representerar standardfelströmmen. Genom att använda funktionen ```fmt.Fprintf()``` med detta objekt kan vi skriva direkt till standardfel.

## Se även:
- [Go-lang.org: Writing to Standard Error](https://golang.org/pkg/fmt/#Fprintf)
- [Medium: Error handling in Go — Part I](https://medium.com/go-walkthrough/go-walkthrough-error-handling-in-go-part-i-89a0f89f7671)
- [Os.File - Go Standard Library Documentation](https://golang.org/pkg/os/#File)