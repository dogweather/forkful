---
title:    "Go: Skapa en tillfällig fil"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Varför skapa temporära filer i Go?

En temporär fil är en fil som endast behövs för en kort stund och sedan kan tas bort. Skapandet av en temporär fil kan vara användbart når man behöver skriva data som endast behövs tillfälligt, till exempel för mellanlagring eller för att hantera temporära tillfälliga filer för ett program. I denna bloggpost kommer jag att visa dig hur du skapar en temporär fil i Go och möjliga användningsområden.

## Så här gör du

För att skapa en temporär fil i Go, behöver du använda "ioutil" paketet tillsammans med funktionen "TempFile". "TempFile" tar två parametrar, sökvägen där filen ska skapas samt ett prefix för filnamnet.

```Go
package main

import (
    "fmt"
    "io/ioutil"
)

func main() {
    // Skapa temporär fil i samma mapp som programmet och prefixet "temp"
    tempFile, err := ioutil.TempFile(".", "temp")

    if err != nil {
        fmt.Println("Kunde inte skapa temporär fil: ", err)
        return
    }

    // Skriv data till filen
    data := []byte("Detta är en temporär fil skapad av Go")
    _, err = tempFile.Write(data)

    if err != nil {
        fmt.Println("Kunde inte skriva till filen: ", err)
        return
    }

    // Stäng filen när vi är klara
    defer tempFile.Close()
    
    // Skriv ut filnamnet för den skapade temporära filen
    fmt.Println("Skapade en temporär fil med namn:", tempFile.Name())
}
```

När du kör detta program så kommer en temporär fil med prefixet "temp" att skapas i samma mapp som ditt program. Den skapade filen kommer att innehålla texten "Detta är en temporär fil skapad av Go". Notera att du även stänger filen när du är klar med hjälp av "defer" funktionen.

## Deep Dive

Det finns flera användbara funktioner som du kan använda tillsammans med "TempFile" i Go. Till exempel kan du ange ett postfix för filnamnet eller ange den mapp där filen ska skapas. Du kan även använda funktionen "TempDir" för att skapa en temporär mapp istället för en fil.

Det är också viktigt att notera att "TempFile" funktionen returnerar en "File" typ, vilket du kan använda för att utföra olika operationer på den skapade filen som t.ex. att läsa eller skriva till den.

# Se även

- [ioutil paketet i Go](https://golang.org/pkg/io/ioutil/)
- [Go dokumentation för TempFile funktionen](https://golang.org/pkg/io/ioutil/#TempFile)
- [En guide för hur man skapar temporära filer i Go](https://www.calhoun.io/creating-a-temporary-file-in-go/)