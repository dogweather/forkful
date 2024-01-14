---
title:                "Go: Att läsa inmatade kommandon från kommandoraden"
simple_title:         "Att läsa inmatade kommandon från kommandoraden"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Att kunna läsa kommandoradsargument i Go är en viktig färdighet för alla som vill kunna skriva kommandoradsapplikationer eller bygga program som kan anropas från kommandoraden.

## Hur man gör

Läsande av kommandoradsargument i Go är enkelt och kan göras med hjälp av standardbiblioteket "flag". Här är en kodexempel tillsammans med det förväntade utdata:

```
package main

import (
	"flag"
	"fmt"
)

func main() {

	// Definiera de argument som du vill ta emot
	argument1 := flag.String("argument1", "default", "första argumentet")
	argument2 := flag.Int("argument2", 0, "andra argumentet")

	// När argumenten är definierade, anropa "flag.Parse()" för att läsa dem från kommandoraden
	flag.Parse()

	// Skriv ut argumenten till konsolen
	fmt.Println("Det första argumentet är: ", *argument1)
	fmt.Println("Det andra argumentet är: ", *argument2)
}
```

Om du exempelvis kör programmet som `go run main.go -argument1=hej -argument2=10` kommer utdatan att se ut så här:

```
Det första argumentet är: hej
Det andra argumentet är: 10
```

Det är viktigt att observera att argumentet måste följas av ett "=" när du anropar programmet från kommandoraden. Om du bara skriver `go run main.go -argument1` utan ett "=" kommer programmet att krascha.

## Djupdykning

Nu när vi vet hur man läser kommandoradsargument är det lämpligt att titta närmare på hur det faktiskt fungerar. När vi skapar variablerna `argument1` och `argument2` använder vi funktionen "String" respektive "Int" från bibloteket "flag". Detta gör att de argumenten kan läsas in från kommandoraden och omgöras till de respektive datatyperna. Därefter använder vi "*argument1" och "*argument2" för att hämta värdet från variablerna. När vi lägger en "*" framför en variabel i Go så hämtas värdet från den variabeln istället för referensen till den. Det betyder att vi får tillgång till det faktiska värdet istället för att ändra värdet på den ursprungliga variabeln.

Det finns också flera andra funktioner som kan användas för att läsa kommandoradsargument, såsom "Bool" för boolean-värden och "Duration" för tidsvärden. Du kan också använda "flag.Args()" för att hämta en lista på alla argument som inte definieras explicit i koden. Detta kan vara användbart för mer flexibilitet i dina program.

## Se även

* [Go Dokumentation om flag-paketet](https://golang.org/pkg/flag/)
* [Officiell Go tutorial för kommandoradsargument](https://golang.org/doc/tutorial/flags)
* [En guide för att läsa osignerade argument i Go](https://www.digitalocean.com/community/tutorials/how-to-use-the-flag-package-in-go)