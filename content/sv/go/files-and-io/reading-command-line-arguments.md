---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:06:33.526259-07:00
description: "Hur man g\xF6r: Go ger direkt\xE5tkomst till kommandoradsargument genom\
  \ paketet `os`, specifikt med hj\xE4lp av `os.Args`, en array av str\xE4ngar. H\xE4\
  r \xE4r ett enkelt\u2026"
lastmod: '2024-03-13T22:44:37.408871-06:00'
model: gpt-4-0125-preview
summary: "Go ger direkt\xE5tkomst till kommandoradsargument genom paketet `os`, specifikt\
  \ med hj\xE4lp av `os.Args`, en array av str\xE4ngar."
title: "L\xE4ser in kommandoradsargument"
weight: 23
---

## Hur man gör:
Go ger direktåtkomst till kommandoradsargument genom paketet `os`, specifikt med hjälp av `os.Args`, en array av strängar. Här är ett enkelt exempel för att komma igång:

```go
package main

import (
    "fmt"
    "os"
)

func main() {
    // os.Args ger åtkomst till råa kommandoradsargument
    fmt.Println("Kommandoradsargument:", os.Args)

    if len(os.Args) > 1 {
        // Loopa igenom argumenten, hoppa över det första (programnamnet)
        for i, arg := range os.Args[1:] {
            fmt.Printf("Argument %d: %s\n", i+1, arg)
        }
    } else {
        fmt.Println("Inga kommandoradsargument tillhandahållna.")
    }
}
```

Ett exempel på utskrift när man kör med `go run yourprogram.go arg1 arg2` kan se ut så här:

```
Kommandoradsargument: [/tmp/go-build123456789/b001/exe/yourprogram arg1 arg2]
Argument 1: arg1
Argument 2: arg2
```

Detta skriver ut alla argument inklusive programnamnet (oftast vid index 0), och itererar sedan över varje tillhandahållet argument, och skriver ut dem. För mer kontrollerad argumenttolkning kan du överväga paketet `flag` för tolkning av kommandoradsoptioner.

## Djupdykning
Historiskt sett är åtkomst av kommandoradsargument en praxis lika gammal som C-programmering, där `argc` och `argv[]` tjänar ett liknande syfte. I Go gör `os.Args` det okomplicerat men avsiktligt grundläggande. För mer komplexa scenarier, såsom hantering av flaggor eller alternativ, erbjuder Go paketet `flag` som ger robusta tolkningsfunktioner. Detta kan ses som ett "bättre" alternativ när din applikation kräver mer än bara positionella argument.

Till skillnad från vissa skriptspråk som erbjuder inbyggd tolkning av kommandoradsargument till associativa arrayer eller objekt, kräver Gos tillvägagångssätt att programmerare antingen manuellt hanterar tolkning med `os.Args` för grundläggande behov eller utnyttjar paketet `flag` för mer avancerade scenarier. Denna design speglar Gophilosofin att hålla kärnspråket enkelt samtidigt som kraftfulla standardbibliotek för vanliga uppgifter tillhandahålls. Även om det kan introducera en liten inlärningskurva för dem som är vana vid inbyggd tolkning, erbjuder det större flexibilitet och uppmuntrar till en djupare förståelse av hantering av kommandoradsargument.
