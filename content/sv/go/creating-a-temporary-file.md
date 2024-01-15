---
title:                "Skapa en tillfällig fil"
html_title:           "Go: Skapa en tillfällig fil"
simple_title:         "Skapa en tillfällig fil"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

Skapandet av temporära filer är en vanlig uppgift inom programmering, särskilt när det gäller att lagra och hantera data på ett tillfälligt sätt. Det kan också vara användbart för att tillfälligt lagra resultatet av en beräkning eller för att hantera tillfälliga systemresurser.

## Hur man gör

För att skapa en temporär fil i Go, behöver vi importera paketet "io/ioutil" som har funktionen "TempFile" som gör just detta. Nedan är ett exempel på hur man skapar en temporär fil och skriver några data till den:

```Go
package main

import (
	"fmt"
	"io/ioutil"
)

func main() {
	// Skapar en temporär fil med prefixet "temp-" i det aktuella arbetskatalogen
	tempFile, err := ioutil.TempFile("", "temp-")
	if err != nil {
		fmt.Println("Kunde inte skapa en temporär fil:", err)
		return
	}
	defer tempFile.Close()

	// Skriver data till den temporära filen
	data := []byte("Det här är data som ska skrivas till den temporära filen.")
	_, err = tempFile.Write(data)
	if err != nil {
		fmt.Println("Kunde inte skriva data till den temporära filen:", err)
		return
	}

	fmt.Println("En temporär fil har skapats:")
	// Skriver ut den temporära filens namn
	fmt.Println(tempFile.Name())
}
```

När vi kör detta program kommer följande att skrivas ut i terminalen:

```
En temporär fil har skapats:
/tmp/temp-182867733
```

Som du kan se så har den temporära filen ett namn som innehåller prefixet som vi valt ("temp-" i detta fall) och en unik identifierare som skapas automatiskt.

## Djupdykning

Förutom att skapa en temporär fil i det aktuella arbetsdirectoryt, så kan vi också specificera en annan mapp där den temporära filen ska skapas. Detta görs genom att ange en sökväg som det första argumentet till "TempFile" funktionen.

Utöver det så finns det också en "TempDir" funktion som används för att skapa en temporär mapp istället för en fil.

En annan viktig sak att komma ihåg är att den temporära filen eller mappen kommer att raderas automatiskt när programmet avslutas. Om du behöver behålla den temporära filen efter att programmet har avslutats, så kan du använda "tempFile.Name()" för att få filens sökväg och flytta eller kopiera den till en annan plats.

## Se även

- [io/ioutil paketets dokumentation på Go.org](https://golang.org/pkg/io/ioutil/)
- [Skapa och använda temporära filer och mappar i Go - Medium.com](https://medium.com/@arpitvshah/create-and-use-temporary-files-and-directories-in-go-25ed069ffe1b)