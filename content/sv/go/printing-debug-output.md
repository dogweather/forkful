---
title:                "Utskrift av felsökningsutmatning"
html_title:           "Go: Utskrift av felsökningsutmatning"
simple_title:         "Utskrift av felsökningsutmatning"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

När man arbetar med att utveckla program eller applikationer i Go, är det viktigt att kunna hitta och åtgärda eventuella fel och buggar. Genom att lägga till debug-utskrifter i koden, kan man få en bättre förståelse för koden och dess beteende, vilket i sin tur kan göra processen med att felsöka enklare och snabbare.

## Hur man gör

För att skriva ut debug-utskrifter i Go, kan vi använda funktionen `fmt.Printf()`. Denna funktion tar två argument - en string med ett format och en lista med värden som ska skrivas ut. Låt oss ta en titt på ett kodexempel:

```Go
package main

import "fmt"

func main() {
    num1 := 10
    num2 := 5
    fmt.Printf("Num1 is %d and num2 is %d.\n", num1, num2)
}
```

I detta exempel skapar vi två variabler, `num1` och `num2`, och sedan skriver vi ut deras värden i en string genom att använda `%d` som placeholders och lista de två variablerna som argument.

När vi kör denna kod får vi följande utskrift:

```
Num1 is 10 and num2 is 5.
```

Detta är ett enkelt exempel, men mängden information som du kan skriva ut och formatera med `fmt.Printf()` är obegränsad.

## Djupdykning

I de flesta fall kommer du troligtvis inte vilja ha debug-utskrifter permanent i din kod. Därför är det viktigt att kunna "toggle" dessa utskrifter utan att behöva ändra koden varje gång.

En bra lösning på detta är att använda `log`-paketet i Go, som ger oss möjlighet att enkelt ställa in en nivå av loggning baserat på åtgärdenivåer (t.ex. `debug`, `info`, `warning`, `error`, etc.).

För att använda log-paketet, måste vi först importera det i vår kod:

```Go
import "log"
```

Sedan kan vi använda funktionen `Print()` för att skriva ut ett meddelande:

```Go
log.Print("Debug message")
```

För att ställa in loggnivån, kan vi använda funktionen `SetOutput()` och ange en fil där logg-meddelanden ska skrivas till:

```Go
log.SetOutput(os.File("logfile.log"))
```

Vi kan också använda `SetFlags()` för att specificera vilken information som ska inkluderas i logg-meddelanden, som t.ex. timestamp eller loggnivå.

En annan användbar funktion är `Printf()`, som fungerar på samma sätt som `fmt.Printf()` men skriver till loggen istället för att skriva ut på skärmen:

```Go
log.Printf("Num1 is %d and num2 is %d.\n", num1, num2)
```

Med hjälp av log-paketet, kan vi enkelt lägga till och ta bort debug-utskrifter från vår kod utan att behöva ändra koden varje gång.

## Se även

- [Go Documentation: Package "log"](https://golang.org/pkg/log/)
- [A Guide to Logging in Go](https://www.ardanlabs.com/blog/2013/11/using-log-package-in-go.html)
- [Debugging Go Code](https://www.calhoun.io/debugging-go-code-from-the-very-basics-to-pitfalls-to-avoid/)