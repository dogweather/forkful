---
title:                "Att få den aktuella datumen"
html_title:           "Go: Att få den aktuella datumen"
simple_title:         "Att få den aktuella datumen"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att få den nuvarande datumen i en program är en vanligt förekommande uppgift för många programmerare. Det kan hjälpa till att hålla reda på tidsstämplar på data, planera uppgifter eller helt enkelt bara visa rätt datum för användare.

## Hur man gör:
Att få den nuvarande datumen i Go är enkelt. Använd bara funktionen "Now()" från paketet "time" och spara den i en variabel. Sedan kan du använda variabeln för att utföra olika åtgärder som att konvertera det till ett visst format eller jämföra med andra datum.

```Go
import "fmt"
import "time"

func main() {
  today := time.Now()
  fmt.Println(today)
}
```
Output:
```
2022-02-17 23:28:47.6610894 +0100 CET
```

## Djupdykning:
I Go, som i många andra programmeringsspråk, är tid en valuta som används för att mäta och jämföra händelser. Det finns också alternativa sätt att få den nuvarande datumen ia program, såsom att använda externa bibliotek eller att anropa systemkommandon. För implementationen av "Now()" funktionen, använder Go kernelns systemklocka och returnerar sedan värdena från den.

## Se även:
- https://golang.org/pkg/time/
- https://www.techonthenet.com/go/commands/time_now.php