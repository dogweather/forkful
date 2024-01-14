---
title:    "Go: Få den aktuella datumet."
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför
Att ha kunskap om hur man får den aktuella datumen är en viktig del av att vara en skicklig Go-programmerare. Det kan användas för att spåra tidsstämplar, hantera löptider och mycket mer.

## Hur man gör det
För att få det aktuella datumet i Go kan du använda "time.Now()" funktionen. Detta ger ett "time.Time" objekt som sedan kan formateras enligt önskemål. Se nedan för ett kodexempel.

```Go
package main
import (
    "fmt"
    "time"
)

func main() {
    // Hämta det aktuella datumet och tiden
    now := time.Now()
    
    // Formatera datumet och tiden
    date := now.Format("2006-01-02") // 02 januari 2006 är den amerikanska standardformatet för datum
    time := now.Format("15:04:05") // 15:04:05 är den amerikanska standardformatet för tiden
    
    // Skriv ut datumet och tiden
    fmt.Println("Aktuellt datum:", date)
    fmt.Println("Aktuell tid:", time)
}
```

Output: 
```
Aktuellt datum: 2021-08-04
Aktuell tid: 12:00:00
```

## Djupdykning
I Go används tid och datum som en datatyp som kallas "time.Time". Detta objekt innehåller många användbara funktioner för att hantera datum och tid, som att lägga till och subtrahera tiden, jämföra två tider och konvertera till olika tidszoner. Det erbjuder också möjligheten att formatera datum och tid enligt användarens önskemål med hjälp av "Format()" funktionen som användes i exemplet ovan.

## Se även
- [A tour of Go](https://tour.golang.org/welcome/1)
- [Go's time package documentation](https://golang.org/pkg/time/)
- [Using time in Go by TutorialEdge](https://tutorialedge.net/golang/go-time-formatting-date-timestamps/)