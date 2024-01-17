---
title:                "Arbeta med csv"
html_title:           "Go: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?
CSV står för Comma-Separated Values och är en vanligt förekommande filformat för att lagra och överföra strukturerad data, vanligtvis i form av en tabell med rader och kolumner. Programmare arbetar med CSV för att enkelt kunna hantera och manipulera data i ett enkelt och läsbart format.

## Hur du gör:
Att arbeta med CSV i Go är enkelt och smidigt, tack vare det inbyggda paketet "encoding/csv". Nedan följer ett exempel på hur man kan läsa in data från en CSV-fil och skriva ut det till konsolen:

```Go
package main

import (
    "encoding/csv"
    "fmt"
    "log"
    "os"
)

func main() {
    // Öppna filen för läsning
    file, err := os.Open("data.csv")
    if err != nil {
        log.Fatal(err)
    }
    defer file.Close()

    // Skapa en ny CSV-läsare från filen
    reader := csv.NewReader(file)

    // Läs in alla rader i filen
    records, err := reader.ReadAll()
    if err != nil {
        log.Fatal(err)
    }

    // Loopa igenom alla rader och skriv ut dem till konsolen
    for _, record := range records {
        fmt.Println(record)
    }
}
```

## Djupdykning:
CSV-filformatet har funnits sedan 1972 och har sedan dess blivit ett populärt sätt att dela och lagra data. Det finns också alternativ till att arbeta med CSV, som till exempel JSON och XML. För att arbeta mer effektivt med CSV-filer kan man använda externa paket som till exempel "github.com/gocarina/gocsv". 

Inuti CSV-filen använder sig Go av det charset "UTF-8" för att hantera speciella tecken och unicode. Det är också möjligt att ange andra teckenuppsättningar genom att ändra inställningarna för teckenkodning.

## Se även:
- [Go-paketet "encoding/csv" dokumentation](https://golang.org/pkg/encoding/csv/)
- [Alternativ för att arbeta med CSV-filer i Go](https://medium.com/@ankitagarg30/top-2-libraries-to-read-and-write-csv-files-in-go-golang-28b3600260c7)