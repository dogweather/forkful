---
title:    "Go: Att få den aktuella datumet."
keywords: ["Go"]
---

{{< edit_this_page >}}

## Varför

Att kunna få den aktuella datumet är en viktig del av många Go-program eftersom det möjliggör att utföra specifika uppgifter baserat på tiden.

## Hur man gör

Att få den aktuella datumet i Go är enkelt med hjälp av tidspaketet, som kommer med en mängd olika funktioner för att hantera tid och datum. Här är ett exempel på hur man kan få den aktuella datumet:

```Go
import "fmt"
import "time"

func main() {
    // Skapa ett tidsobjekt med den aktuella tiden
    t := time.Now()

    // Använd formatfunktionen för att få datumet i önskat format
    fmt.Printf("Den aktuella datumet är %02d/%02d/%04d\n", t.Day(), t.Month(), t.Year())
}
```

<strong>Output:</strong>

```
Den aktuella datumet är 28/06/2021
```

Att använda sig av tidsformatet i formatfunktionen möjliggör för programmeraren att anpassa formatet på datumet baserat på deras behov.

## Djupdykning

Det finns flera olika funktioner och metoder i tidspaketet som kan användas för att få den aktuella datumet. En av dem är `Now()` som vi använde i vårt exempel ovan. Det finns också `Date()` som kan användas för att skapa en tidsstämpel med ett specifikt datum, månad och år. Du kan också använda `Parse()` för att konvertera en sträng till ett tidsobjekt och sedan använda `Format()` för att få datumet i det formatet du önskar.

## Se även

- [Go tidspaket dokumentation](https://golang.org/pkg/time/)
- [Go tid och datum exempel](https://gobyexample.com/time)
- [Go tidshanteringstips](https://www.calhoun.io/working-with-dates-and-times-in-go/)

Se till att undersöka fler funktioner och metoder tillgängliga i tidspaketet för att få en bättre förståelse av hur du kan hantera datum och tid i dina Go-program. Lycka till med programmeringen!