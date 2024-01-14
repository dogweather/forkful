---
title:    "Go: Att skriva en textfil"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att kunna skriva textfiler är en viktig färdighet för alla som arbetar med Go-programmering. Det tillåter dig att spara data på ett enkelt och effektivt sätt. Om du inte redan vet hur man gör det, så är det enkelt och här kommer vi att titta på hur man skriver en textfil med Go.

## Hur man gör

För att skriva en textfil med Go behöver du först importera "os" paketet, som ger tillgång till operativsystemsfunktioner. Sedan använder du funktionen `OpenFile` för att öppna eller skapa en fil. Filen måste också stängas när du är färdig för att spara ändringarna. Här är ett kodexempel:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	// Skapa eller öppna en fil för skrivning
	file, err := os.OpenFile("textfil.txt", os.O_CREATE|os.O_WRONLY, 0644)
	if err != nil {
		fmt.Println(err)
		return
	}
	defer file.Close()
	
	// Skriv till filen
	_, err = file.WriteString("Det här är en textfil skriven i Go!")
	if err != nil {
		fmt.Println(err)
		return
	}
	fmt.Println("Textfilen har blivit skapad och innehållet har skrivits!")
}
```

När du kör detta program kommer en textfil med namnet "textfil.txt" att skapas i den aktuella mappen och den angivna texten kommer att skrivas till filen.

## Deep Dive

Det finns flera parametrar som kan användas för att anpassa hur en textfil skrivs med Go. En av dessa är flaggan `os.O_APPEND` som tillåter dig att lägga till text till slutet av en befintlig fil. Du kan också använda `file.WriteString` flera gånger för att skriva flera rader till filen.

För att lära dig mer om hur du skriver textfiler med Go, rekommenderar vi att du tittar på dokumentationen för "os" paketet och experimenterar med olika parametrar och funktioner.

## Se även

Här är några användbara länkar för att hjälpa dig att utforska ämnet ytterligare:

- [Officiell dokumentation för "os" paketet (på svenska)](https://golang.org/pkg/os/)
- [Go-skolan: Filhantering (på svenska)](https://golang.org/doc/tutorial/files)
- [Go-boken: Skriva och läsa textfiler (på svenska)](https://www.golang-book.com/books/intro/10)