---
title:                "Go: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför
Det är ofta nödvändigt att läsa in data från en textfil för att kunna utföra olika operationer i ett program. Det kan till exempel vara för att bearbeta stora mängder data eller för att kunna presentera information till användaren.

## Hur man gör
För att läsa in en textfil i Go behöver vi först öppna filen med hjälp av funktionen `Open()` från paketet `os`. Vi behöver även ett `Scanner`-objekt för att kunna läsa in rad för rad från filen. Sedan kan vi använda en `for`-loop för att läsa in alla rader och utföra önskade operationer.

```Go
package main

import (
    "fmt"
    "os"
    "bufio"
)

func main() {
    // Öppna filen för läsning
    file, err := os.Open("data.txt")
    if err != nil {
        fmt.Println("Kunde inte öppna filen:", err)
        return
    }
    defer file.Close()

    // Skapa en ny Scanner för att läsa in filen
    scanner := bufio.NewScanner(file)

    // Läs in varje rad och skriv ut den till konsolen
    for scanner.Scan() {
        fmt.Println(scanner.Text())
    }

    // Kontrollera eventuella fel under inläsning
    if err := scanner.Err(); err != nil {
        fmt.Println("Kunde inte läsa filen:", err)
        return
    }
}

// Output:
// Rad 1
// Rad 2
// Rad 3
// Rad 4
```

## Djupdykning
För att kunna läsa in mer komplexa textfiler, som till exempel CSV-filer, kan det vara bra att använda sig av paketet `encoding/csv`. Genom att definiera lämpliga fält och inställningar kan vi sedan enkelt läsa in och manipulera data från filen.

```Go
package main

import (
    "fmt"
    "os"
    "encoding/csv"
)

func main() {
    // Öppna filen för läsning
    file, err := os.Open("data.csv")
    if err != nil {
        fmt.Println("Kunde inte öppna filen:", err)
        return
    }
    defer file.Close()

    // Skapa en ny Reader för att läsa in filen
    reader := csv.NewReader(file)

    // Läs in alla rader och skriv ut dem till konsolen
    records, err := reader.ReadAll()
    if err != nil {
        fmt.Println("Kunde inte läsa filen:", err)
        return
    }

    for _, record := range records {
        for _, field := range record {
            fmt.Print(field, " ")
        }
        fmt.Println()
    }
}

// Output:
// Kolumn 1 Kolumn 2 Kolumn 3
// Kolumn 4 Kolumn 5 Kolumn 6
```

## Se även
- Go Dokumentation: https://golang.org/doc/
- Gophercises: https://gophercises.com/
- The Go Blog: https://blog.golang.org/