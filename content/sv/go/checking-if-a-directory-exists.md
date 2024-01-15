---
title:                "Kontrollera om en mapp existerar"
html_title:           "Go: Kontrollera om en mapp existerar"
simple_title:         "Kontrollera om en mapp existerar"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför
Kanske är du en utvecklare som vill skapa ett program som kan kolla om en specifik mapp finns på ditt system. Kanske vill du ha en extra nivå av säkerhet för att undvika att din kod kraschar om en mapp inte finns. I den här artikeln kommer vi att gå igenom hur du kan checka om en mapp finns i Go, det populära programmsspråket som lämpar sig för allt från småskaliga projekt till stora system.

## Så här gör du
För att kolla om en mapp finns i Go behöver vi använda oss av paketet "os". Genom att importera detta paket kan vi få tillgång till funktioner som hjälper oss att navigera och manipulera filsystemet.

```Go 
import "os"

// Kontrollera om en mapp finns
if _, err := os.Stat("min_mapp"); err == nil{
    // Mappen finns!
    fmt.Println("Mappen finns")
} else {
    // Mappen finns inte, hantera detta i din kod
    fmt.Println("Mappen finns inte")
}
``` 

I koden ovan använder vi funktionen "Stat" från "os" för att kolla om mappen "min_mapp" finns. Om det inte finns något fel (err == nil) betyder det att mappen existerar och vi kan göra åtgärder med den. Om däremot ett fel uppstår betyder det att mappen inte finns och vi kan hantera detta i vår kod.

## Deep Dive
Om du vill ha lite mer kontroll över hur du kollar om en mapp finns kan du använda dig av funktionen "os.Stat" som vi använde i vårt exempel ovan. Denna funktion returnerar en datastruktur som innehåller information om filens eller mappens namn, storlek, skapad datum och andra attribut. Du kan sedan använda detta för att göra olika kontroller och manipulationer.

Det är också värt att notera att När vi använder funktionen "Stat" kan vi också få information om fel som kan uppstå, till exempel om filen inte finns eller om du inte har tillräckliga rättigheter att komma åt filsystemet.

## Se även
- ["os" Paketet i Go](https://golang.org/pkg/os/)
- [Undersökning av filer och mappar med Go](https://golangbyexample.com/golang-check-if-file-directory-exists/)