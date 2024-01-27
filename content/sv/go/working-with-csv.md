---
title:                "Arbeta med csv"
date:                  2024-01-19
html_title:           "Arduino: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?
CSV står för "Comma-Separated Values". Programmerare jobbar med CSV för att enkelt utbyta data mellan olika system. Det är ett simpelt, textbaserat format som används överallt.

## How to:
Låt oss dyka rätt in. Först, läsa från en CSV-fil:

```go
package main

import (
    "encoding/csv"
    "fmt"
    "os"
)

func main() {
    file, err := os.Open("data.csv")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    reader := csv.NewReader(file)
    records, err := reader.ReadAll()
    if err != nil {
        panic(err)
    }

    for _, record := range records {
        fmt.Println(record)
    }
}
```

Exempel output:
```
["förnamn", "efternamn", "ålder"]
["Anna", "Andersson", "28"]
["Lars", "Larsson", "34"]
```

Och så, skriva till en CSV-fil:

```go
package main

import (
    "encoding/csv"
    "os"
)

func main() {
    records := [][]string{
        {"förnamn", "efternamn", "ålder"},
        {"Anna", "Andersson", "28"},
        {"Lars", "Larsson", "34"},
    }

    file, err := os.Create("ny_data.csv")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    writer := csv.NewWriter(file)
    defer writer.Flush()

    for _, record := range records {
        if err := writer.Write(record); err != nil {
            panic(err)
        }
    }
}
```

Efter att koden körs finns filen `ny_data.csv` med datan vi skrev.

## Deep Dive
CSV har använts sedan tidigt 1970-tal. Alternativ inkluderar JSON och XML, men CSV prioriteras ofta för dess enkelhet och låg overhead. Go's standardbibliotek `encoding/csv` hanterar RFC 4180-standardiserade CSV-filer, vilket inkluderar subtiliteter som att hantera nya rader och citattecken inom fält.

## See Also
- Go-dokumentation om `encoding/csv`: https://pkg.go.dev/encoding/csv
- RFC 4180, standarden för CSV-filer: https://tools.ietf.org/html/rfc4180
- CSV vs JSON vs XML: https://www.dataedo.com/kb/data-glossary/what-is-csv-json-xml
