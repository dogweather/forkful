---
title:                "Kontrollera om en mapp finns"
html_title:           "Go: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en mapp (directory) existerar är en vanlig uppgift för Go-programmerare. Detta betyder helt enkelt att man söker efter en specifik mapp på en dator eller server för att se om den finns eller inte. Detta är viktigt för att kunna navigera och arbeta med filer och mappar på ett effektivt sätt.

## Hur?
För att kontrollera om en mapp existerar i Go använder man funktionen `os.Stat()` tillsammans med mappens sökväg. Om mappen finns kommer funktionen att returnera en `os.FileInfo` struktur som innehåller information om mappen. Om mappen inte finns kommer funktionen att returnera ett felmeddelande. Här är ett exempel:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	// Sökvägen till mappen
	dir := "C:/Users/Johanna/Documents"

	// Kör funktionen för att kontrollera mappen
	if _, err := os.Stat(dir); !os.IsNotExist(err) {
		// Om mappen finns, skriv ut ett meddelande
		fmt.Println("Mappen finns!")
	} else {
		// Om mappen inte finns, skriv ut ett felmeddelande
		fmt.Println("Mappen finns inte!")
	}
}
```

Output: "Mappen finns!"

## Djupdykning
Kontroll av mappars existens har funnits med i Go sedan den första versionen släpptes på 2000-talet. Det finns även andra sätt att kontrollera mappars existens, som att använda `os.MkdirAll()` för att skapa en mapp om den inte redan finns.

Det är också möjligt att använda paketet `path/filepath` för att söka efter en mapp baserat på dess namn eller sökväg. Detta kan vara användbart om man inte vet exakt var mappen kommer att finnas, men vet hur den kommer att heta.

## Se även
- [Officiell dokumentation för os.Stat()](https://golang.org/pkg/os/#Stat)
- [Exempel på att kontrollera mappars existens i Go](https://goplay.space/#DTWYwhNj7CV)
- [Mer om mappar och filer i Go](https://www.golang-book.com/books/intro/8)