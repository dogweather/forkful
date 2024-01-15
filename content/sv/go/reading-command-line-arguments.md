---
title:                "Läsa kommandoradsargument"
html_title:           "Go: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Ek: Varför ska man läsa kommandoradsargument?

Att kunna läsa och hantera kommandoradsargument är en mycket användbar färdighet för alla som arbetar med programutveckling eller scriptning. Det ger dig möjlighet att interagera med dina program på ett enkelt och snabbt sätt, och förstå deras beteende bättre.

## Varför

Att läsa kommandoradsargument är en grundläggande kunskap som alla Go-utvecklare bör ha. Det gör det möjligt för dig att anpassa dina program och få ut mer information från dem. Det är också en vanlig färdighet för många andra programmeringsspråk.

## Hur man gör

För att läsa kommandoradsargument i Go använder vi paketet `os` och dess `Args` variabel. Här är ett enkelt exempel på ett program som tar emot och skriver ut ett kommandoradsargument:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Första argumentet är alltid namnet på exekverbar fil, så vi börjar från andra argumentet
    for i, arg := range os.Args[1:] {
        fmt.Printf("Argument %d: %s\n", i, arg)
    }
}
```

Om vi kör programmet med `go run main.go hello world` kommer följande utdata att genereras:

`Argument 0: hello`

`Argument 1: world`

Det första argumentet, `hello`, är namnet på den exekverbara filen och ignoreras därför oftast. Resten av argumenten är de som vi anger vid körning av programmet. 

## Djupdykning

I exemplet ovan använde vi `Args` variabeln från `os` paketet för att läsa kommandoradsargumenten. Denna variabel är en `[]string` och innehåller en array av alla argument som används vid körningen av programmet.

Vi kan också använda den inbyggda funktionen `len()` för att få antalet argument som har angetts. Genom att använda denna information kan vi bygga mer komplexa program som läser och behandlar argumenten på olika sätt.

## Se även

- [Dokumentationen för paketet `os` i Go](https://golang.org/pkg/os/)
- [En guide till kommandoradsargument i Go](https://tutorialedge.net/golang/parsing-command-line-arguments-golang/)