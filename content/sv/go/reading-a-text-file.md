---
title:                "Läsa en textfil"
html_title:           "Fish Shell: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa en textfil innebär i grund och botten att öppna och bearbeta innehållet i en textfil med hjälp av en programmeringsspråk. Programmerare gör det ofta för att få tag i data, manipulera och använda det i sina appar eller webbsidor.

## Hur det görs:
Här är ett enkelt exempel:
```Go
package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func main() {
	file, err := os.Open("test.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		fmt.Println(scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
}
```
När du kör den här koden, kommer den att skriva ut innehållet i 'test.txt' till konsolen:
```Go
Hello, World!
This is a test file.
```

## Djupdykning
Historiskt sett har läsning av textfiler alltid varit en kärnfunktion i alla programmeringsspråk. Go-programspråket erbjuder inbyggd support för att öppna och bearbeta filer på ett mycket effektivt sätt tack vare dess exceptionell hantering av fel och buffertjänster.
Det finns flera sätt att läsa en textfil i Go: du kan använda `ioutil.ReadFile`, `os.Open` följt av `bufio.Scanner`, som jag nämnde ovan, eller `os.Open` följt av `ioutil.ReadAll`. Varje metod har sina egna fördelar och nackdelar, beroende på filens storlek och dina specifika behov.
Implementationen som jag visade ovan öppnar filen, skapar en ny scanner, läser filen rad för rad och skriver ut varje rad till konsolen. Om ett fel uppstår under processen rapporteras det strax.

## Se även:
Du kan hitta mer information om filhantering i Go i följande länkar:
* [Go by Example: Reading Files](https://gobyexample.com/reading-files)
* [Go Docs: Package os](https://golang.org/pkg/os/)
* [Go Docs: Package bufio](https://golang.org/pkg/bufio/)