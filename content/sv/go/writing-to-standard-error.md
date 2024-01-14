---
title:    "Go: Skrivning till standardfel"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Varför

Att skriva till standardfel är en viktig del av Go-programmering eftersom det ger dig möjlighet att hantera och rapportera fel i ditt program. Det gör det också lättare att felsöka och förbättra koden.

## Hur man gör

För att skriva till standardfel i Go använder vi funktionen `fmt.Fprintf()`. Det är en praktisk funktion som tar emot flera argument och skriver dem till standardfel. Här är ett kodexempel:

```Go
package main

import "fmt"

func main() {
    name := "Johan"
    fmt.Fprintf(os.Stderr, "Hej %s, det uppstod ett fel!", name)
}
```

Det första argumentet till `fmt.Fprintf()` är den plats där vi vill skriva, i det här fallet `os.Stderr` för standardfel. Det andra argumentet är en sträng som innehåller formatmallar och det tredje argumentet är värdet som ska skrivas ut. I vårt exempel, `%s` representerar variabeln `name`.

När du kör detta program kommer du att se följande output på din terminal:

```
Hej Johan, det uppstod ett fel!
```

## Djupdykning

För att förstå mer om att skriva till standardfel i Go är det viktigt att förstå skillnaden mellan `fmt.Fprintf()` och `fmt.Printf()` funktionerna. Båda gör samma sak, men med en liten skillnad - `fmt.Fprintf()` skriver till en plats som du anger medan `fmt.Printf()` skriver till standardutdata (din terminal).

Det finns också ytterligare alternativ för `fmt.Fprintf()` som gör det möjligt att skriva till andra platser än standardfel, som till exempel en fil. Du kan läsa mer om detta i Go-dokumentationen.

## Se även

- [Go-dokumentation för fmt-pakethantering](https://golang.org/pkg/fmt/#Fprintf)
- [En guide till standard biblioteket i Go](https://medium.com/rungo/everything-you-need-to-know-about-packages-in-go-b8bac62b74cc)
- [En interaktiv övning för att lära dig Go](https://tour.golang.org/welcome/1)