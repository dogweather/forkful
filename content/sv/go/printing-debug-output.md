---
title:                "Go: Utskrift av felsökningsutdata"
programming_language: "Go"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva kod är en process som är full av utmaningar och ibland kan det vara svårt att hitta fel och buggar i vår kod. För att enkelt hitta och åtgärda dessa problem kan vi använda oss av debug-utskrifter. I denna blogginlägg ska vi utforska hur man kan använda debug-utskrifter i Go-programmering.

## Hur man gör

För att skriva ut debug-meddelanden i Go kan vi använda oss av funktionen `fmt.Printf()`. Denna funktion används för att skriva ut en formaterad sträng till standardutdata. Låt oss ta en titt på ett exempel där vi behöver debugga vår kod:

```Go
package main

import "fmt"

func main() {
    total := 0
    for i := 0; i < 10; i++ {
        total += i
    }
    fmt.Printf("Totalt värde: %d\n", total)
}
```

I detta exempel skriver vi ut värdet på den totala summan i vår loop. Detta kan vara en hjälpsam utskrift för att följa värdet på `total` variabeln och se om det stämmer med våra förväntningar.

Det finns också en annan funktion i Go som heter `log.Print()`. Denna funktion skriver ut en sträng till standardutdata, men dessutom inkluderar den tidsstämpel och filinformation om var utskriften skedde. Detta kan vara särskilt användbart när man debuggar flera delar av kod och vill hålla reda på vilka utskrifter som är relaterade till vilken del av koden.

## Fördjupning

Att använda debug-utskrifter i Go kan vara en enkel och effektiv metod för att hitta och åtgärda buggar. Dock bör man tänka på att det kan vara lätt hänt att glömma bort att ta bort utskrifterna innan man skickar in sin kod för produktion. Detta kan leda till oönskade utskrifter och eventuellt minska prestandan på vår applikation.

Det finns också andra sätt att debugga vår kod, som t.ex. att använda debugger-verktyg eller loggningsramverk. Det är viktigt att hitta den metod som passar bäst för vår kod och arbetssätt.

## Se också

- [Officiell Go-dokumentation om fmt](https://golang.org/pkg/fmt/)
- [How to Debug Go Programs](https://www.digitalocean.com/community/tutorials/how-to-debug-go-programs)
- [Debugging Techniques in Go](https://medium.com/@enricofoltran/debugging-techniques-in-go-d337195fc865)