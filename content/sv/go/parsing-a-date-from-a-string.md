---
title:                "Analysera ett datum från en sträng"
html_title:           "Kotlin: Analysera ett datum från en sträng"
simple_title:         "Analysera ett datum från en sträng"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att tolka ett datum från en sträng i programmering handlar om att omvandla en textrepresentation av ett datum till ett standardiserat datumformat. Detta är vanligtvis nödvändigt när vi behöver manipulera, jämföra, eller utföra operationer på data baserade på datum.

## Så här gör du:

För att tolka ett datum från en sträng i Go, använd den inbyggda ```time```-paketen funktionen ```Parse```. 

```Go
package main
  
import (
    "fmt"
    "time"
)

func main() {
    const datumFormat = "2006-01-02"
    datumSträng := "2021-09-15"
  
    datum, fel := time.Parse(datumFormat, datumSträng)
    if fel != nil {
       fmt.Println(fel)
    }
  
    fmt.Println(datum)
}
```

Detta kommer att skriva ut:

```Go
2021-09-15 00:00:00 +0000 UTC
```

## Djupdykning

Historiskt sett har datumtolkning varit ett svårt problem för programmerare på grund av skillnader i datumformat över länder och regioner. Som ett alternativ till ```time.Parse```, kan man använda ```time.ParseInLocation``` för att tolka ett datumsträng med hänsyn till en specifik tidszon.

Implementationsdetaljerna för datumtolkning i Go handlar om att använda layoutsträngar för att kommunicera önskat format. Go använder en unik, men intuitiv, representation: den använder faktiska tider (specifikt den UTC-tiden för måndagen 2 januari kl. 15:04:05 2006) för att skapa strängformat.

## Se också:

- Go Docs: [The `time` package](https://golang.org/pkg/time/)
- Go Blog: ["The Go time package"](https://blog.golang.org/two-go-talks-unicode-shebang)
- StackOverflow: [Parsing dates from strings in Go](https://stackoverflow.com/questions/14106541/parsing-date-string-in-go)