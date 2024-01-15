---
title:                "Läsa en textfil"
html_title:           "Go: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att läsa och förstå innehållet i en textfil är en viktig del av programmering. Genom att kunna läsa en textfil kan man få tillgång till data som kan användas i ens program och skapa en interaktiv upplevelse för användaren. Därför är det viktigt för alla programmerare att ha kunskap om hur man läser en textfil i Go.

## Hur man gör

För att läsa en textfil i Go behöver man först öppna filen med hjälp av funktionen `Open()` från paketet `os`. Sedan kan man läsa filen byte för byte eller rad för rad med hjälp av läsare som till exempel `Read()`. Till exempel, om vi vill läsa en fil som heter "textfil.txt" så skulle koden se ut såhär:

```Go
package main

import (
    "os"
    "fmt"
)

func main() {
    // Öppna filen "textfil.txt"
    fil, err := os.Open("textfil.txt")
    if err != nil {
        fmt.Println("Kunde inte öppna filen:", err)
    }
    // Läs filen rad för rad
    buff := make([]byte, 1024) // Buffert för att det ska gå snabbare
    for {
        antalRead, err := fil.Read(buff)
        if err != nil {
            // Om läsningen är klar så avsluta
            if antalRead == 0 {
                break
            }
            // Om det var ett annat fel än "EOF" så skriv ut felet
            fmt.Println("Kunde inte läsa filen:", err)
        }
        // Skriv ut innehållet i bufferten
        fmt.Println(string(buff[:antalRead]))
    }
    // Stäng filen när vi är klara med den
    fil.Close()
}
```

Om vi till exempel skulle ha en textfil som heter "textfil.txt" med följande innehåll:

```
Hello World!
This is a text file.
```

Så skulle koden ovan ge följande utskrift:

```
Hello World!
This is a text file.
```

## Djupdykning

När vi läser en textfil med hjälp av `Read()` så får vi tillbaka en array av bytes. För att kunna läsa och tolka texten behöver vi omvandla dessa bytes till en sträng. Det vanligaste sättet att göra detta är genom att använda funktionen `string()`.

Ungefär såhär fungerar det:

1. Vi får en array med bytes från `Read()` som ser ut såhär: `[72 101 108 108 111 32 87 111 114 108 100 33]`
2. Varje nummer representerar en bokstav enligt ASCII-tabellen. Så nummer 72 blir H, 101 blir e, osv.
3. Genom att använda funktionen `string()` på arrayen så omvandlas den till en sträng och vi får "Hello World!" som vi kan skriva ut.

Genom att förstå hur Go behandlar bytes och kunna omvandla dem till strängar har man en grundläggande kunskap för att läsa och tolka data från en fil.

## Se även

- [Go's dokumenation om filhantering](https://golang.org/pkg/os/)
- [En tutorial om hur man öppnar, läser och skriver till en textfil i Go](https://www.digitalocean.com/community/tutorials/how-to-read-and-create-files-in-go)