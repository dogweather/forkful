---
title:                "Go: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Har du någonsin behövt kolla om en viss mapp eller katalog finns på din dator? Det kan vara till hjälp när du behöver hantera filer eller göra systemuppgraderingar som kräver denna information. Go-språket erbjuder en enkel och effektiv metod för att kontrollera om en mapp eller katalog existerar. I den här blogginlägget ska vi gå igenom varför detta är användbart och hur man gör det på ett effektivt sätt.

## Hur man gör

För att kontrollera om en mapp eller katalog existerar i Go-språket, använder vi funktionen `os.Stat` tillsammans med path/filepath-paketet. Detta kommer att ge oss information om sökvägen som vi anger, inklusive om den är en mapp eller katalog. Nedan är ett exempel på kod som visar detta:

```Go
package main

import (
	"fmt"
	"os"
	"path/filepath"
)

func main() {
	path := "/Users/johndoe/Documents"

	// Kontrollera om sökvägen existerar
	if _, err := os.Stat(path); err != nil {
		// Om fel, skriv ut felmeddelande
		fmt.Println("Kunde inte hitta mappen eller katalogen.")
	} else {
		// Om ingen fel, skriv ut meddelande om att sökvägen existerar
		fmt.Println("Mappen eller katalogen finns.")
	}
}
```

Detta kodexempel använder `if`-satsen för att kontrollera om det finns något fel när vi anropar `os.Stat` med vår sökväg. Om det inte finns något fel, betyder det att sökvägen existerar och vi skriver ut ett meddelande om det.

## Djupdykning

Djupdykningen i detta ämne handlar om förståelsen av funktionen `os.Stat` och hur det hjälper till att kontrollera om en sökväg existerar. `os.Stat` returnerar ett `FileInfo`-objekt som innehåller all information om sökvägen som vi behöver kontrollera. Det ger också möjlighet att få åtkomst till filens storlek, ägarinformation, ändringsdatum och mycket mer. Detta gör det till ett kraftfullt verktyg för filhantering i Go-språket.

## Se även

- [Go-paketet os](https://golang.org/pkg/os)
- [Funktionen os.Stat](https://golang.org/pkg/os/#Stat)
- [Path/filepath-paketet](https://golang.org/pkg/path/filepath)
- [FileInfo-objekthandling i Go](https://medium.com/@ankushagarwal/fileinfo-object-handling-in-golang-9edcddb60ba)
- [En guide till filhantering i Go](https://www.golangprograms.com/go-language/golang-file-directory-examples.html)