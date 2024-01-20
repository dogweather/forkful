---
title:                "Att skapa en tillfällig fil"
html_title:           "Bash: Att skapa en tillfällig fil"
simple_title:         "Att skapa en tillfällig fil"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skapa en temporär fil innebär att man skapar en fil i datorns minne som kommer att raderas efter att programmet körs. Programmerare gör detta för att temporärt lagra data under programkörning utan att belasta huvudlagringen eller skapa bestående skräp.

## Hur gör man:
Här är ett exempel på hur du skapar en temporär fil med Go.
```Go
package main

import (
	"io/ioutil"
	"log"
)

func main() {
	tempFile, err := ioutil.TempFile("temp", "go-temp")
	if err != nil {
		log.Fatal(err)
	}

	defer tempFile.Close()

	log.Println("En temporär fil har skapats:", tempFile.Name())
}
```
När du kör koden ovan kommer detta att vara utmatningen:
```
2022/04/03 12:34:56 En temporär fil har skapats: temp/go-temp123456
```

## Djupdykning
Historiskt sett har temporära filer varit en viktig del av datorprogrammering. De erbjuder snabb läs- och skrivåtkomst jämfört med att skicka data över nätverket. Under Go:s utformning har detta traditionella koncept bibehållits, och skapande av temporära filer har gjorts smidigt och enkelt.

Alternativ till att använda temporära filer kan inkludera användning av databaser eller delat systemminne. Men dessa lösningar kan vara överdrivet komplexa i jämförelse, och temporära filer ger en bra mjuk övergång för data under programkörning.

Temporära filer i Go skapas genom att generera randomiserade filnamn, för att minska risken för konflikter. Filerna lagras i systemets standardkatalog för temporära filer. 

## Se även
Källor för vidare läsning:
- Go by Example: Temporary Files and Directories (https://gobyexample.com/temporary-files-and-directories)
- Go Documentation: Package ioutil (https://pkg.go.dev/io/ioutil)
- Go Blog: Defer, Panic, and Recover (https://blog.golang.org/defer-panic-and-recover)