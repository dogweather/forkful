---
title:                "Go: Arbeta med json"
simple_title:         "Arbeta med json"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/working-with-json.md"
---

{{< edit_this_page >}}

## Varför

JSON, eller JavaScript Object Notation, är en vanlig struktur för datautbyte mellan system och applikationer. Det är lätthanterligt och flexibelt, vilket gör det till en populär metod för att överföra och lagra data. Om du arbetar med Go-programmering, kan det vara användbart att veta hur man hanterar JSON för att kunna integrera med andra system och applikationer.

## Så här gör du

För att arbeta med JSON i Go kommer vi att använda ett inbyggt paket som heter "encoding/json". Detta paket ger oss funktioner för att enkelt konvertera data till och från JSON-formatet. Låt oss titta på ett enkelt exempel:

```Go
package main

import (
	"encoding/json"
	"fmt"
)

type Person struct {
	Name  string
	Age   int
	Email string
}

func main() {

	person := Person{
		Name:  "Anna",
		Age:   25,
		Email: "anna@example.com",
	}

	jsonData, err := json.Marshal(person)
	if err != nil {
		fmt.Println(err)
	}

	fmt.Println(string(jsonData)) // output: {"Name":"Anna","Age":25,"Email":"anna@example.com"}
}
```

Vi börjar med att definiera en "Person" struktur och skapar sedan ett objekt med namn, ålder och e-postadress. Sedan använder vi "json.Marshal()" funktionen för att konvertera vår "person" till ett JSON-format. Om allt går väl kommer vi att få en []byte-array som innehåller JSON-data. Genom att använda "string()" funktionen kan vi konvertera []byte till en sträng och skriva ut den på terminalen.

Men vad händer om vi behöver hämta data från en JSON-fil? Inga problem, vi kan använda "json.Unmarshal()" funktionen för att konvertera JSON-data till vår struktur. Låt oss titta på ett exempel:

```Go
package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
)

type Book struct {
	Title       string
	Author      string
	PublishedAt string `json:"published_at"`
}

func main() {

	jsonData, _ := ioutil.ReadFile("book.json")

	var book Book
	err := json.Unmarshal(jsonData, &book)
	if err != nil {
		fmt.Println(err)
	}

	fmt.Println(book) // output: {The Alchemist Paulo Coelho 1988-01-01}
}
```

Vi börjar med att läsa innehållet från en JSON-fil till en []byte-array med hjälp av "ioutil.ReadFile()" funktionen. Sedan deklarerar vi vår "Book" struktur och använder "json.Unmarshal()" funktionen för att fylla vår struktur med data från JSON-filen. Observera att vi även har lagt till en "tag" för fältet "PublishedAt" för att berätta för Go att det matchar med "published_at" i JSON-filen.

## Fördjupning

Genom att använda "encoding/json" paketet kan vi också hantera mer komplexa JSON-strukturer och hantera hantering av fel och ogiltig data. Det finns också möjlighet att använda "json.Decoder" och "json.Encoder" för att hantera streaming av stora JSON-datafiler. För mer information, läs dokumentationen för "encoding/json" paketet.

## Se också

- [The JSON Website](https://www.json.org/)
- [Go Language Specification - Encoding/json](https://golang.org/pkg/encoding/json/)
- [Learn JSON in 10 Minutes](https://www.freecodecamp.org/news/learn-json-in-10-minutes/)