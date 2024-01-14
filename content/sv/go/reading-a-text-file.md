---
title:                "Go: Läsning av en textfil"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att läsa en textfil kan vara ett användbart verktyg för att hantera data och information. Det kan också vara en viktig del av många Go-programmerares arbetsflöde. I denna bloggpost kommer vi att utforska hur man kan läsa en textfil med hjälp av Go-programmeringsspråket.

## Hur man gör det

Först och främst behöver vi importera paketet "os" för att kunna hantera filer i Go. Sedan kan vi öppna en textfil med hjälp av "Open" funktionen från paketet "os", som tar två argument - filnamn och önskat läge.

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // öppna textfilen med läget "r" (för läsning)
    file, err := os.Open("textfil.txt")
    // kontrollera om det uppstod ett fel
    if err != nil {
        fmt.Println(err)
        return
    }
    // stäng filen vid slutet av funktionen
    defer file.Close()

    // läs innehållet i filen en rad i taget
    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        // skriv ut varje rad i filen
        fmt.Println(scanner.Text())
    }

    // kontrollera om det uppstod ett fel under skanningen
    if err := scanner.Err(); err != nil {
        fmt.Println(err)
        return
    }
}
```

Om vi använder en textfil med innehållet "Hej världen!", kommer outputen att bli:

```
Hej världen!
```

## Djupdykning

För att läsa en fil från en annan källa än filsystemet, som exempelvis en HTTP-förfrågan, kan vi använda "NewReader" funktionen från "bufio" paketet istället för "os.Open". Vi kan också använda "ReadAll" funktionen för att läsa in hela filen på en gång, istället för att läsa en rad i taget.

För att hantera stora filer kan vi använda "Reader" och "Writer" från "io" paketet för att göra läsningen och skrivningen mer effektiv.

## Se även

- [Officiell Go-dokumentation för filhantering](https://golang.org/pkg/os/)
- [Go By Example - Filhantering](https://gobyexample.com/reading-files)