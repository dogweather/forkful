---
title:                "Arbeta med json"
html_title:           "Go: Arbeta med json"
simple_title:         "Arbeta med json"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/working-with-json.md"
---

{{< edit_this_page >}}

## Varför
 JSON (JavaScript Object Notation) är ett vanligt format för att lagra och överföra data. Det är lättläst för både människor och datorer, vilket gör det till ett populärt val för hantering av data i programmering. Att kunna hantera JSON-effektivt är en viktig färdighet för alla som arbetar med Go-programmering.

## Hur man gör det
Att arbeta med JSON i Go kan verka skrämmande vid första anblicken, men det är faktiskt ganska enkelt. Här är några exempel på hur man kan använda Go för att läsa och skriva JSON-data.

### Läsning av JSON-data
För att läsa in JSON-data från en fil i Go, kan vi använda paketet `encoding/json` och dess funktion `Unmarshal()`. Se det här exemplet:

```Go
package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
)

type User struct {
	Name  string `json:"name"`
	Email string `json:"email"`
}

func main() {
	// Läs in JSON-filen
	raw, err := ioutil.ReadFile("user.json")
	if err != nil {
		fmt.Println(err.Error())
		return
	}

	// Skapa en variabel för att lagra vår data
	var user User

	// Använd Unmarshal() för att avkoda JSON till user strukturen
	err = json.Unmarshal(raw, &user)
	if err != nil {
		fmt.Println(err.Error())
		return
	}

	// Accessa data med hjälp av vår "user" variabel
	fmt.Println("Användarnamn:", user.Name)
	fmt.Println("Email:", user.Email)
}
```
För detta exempel, anta att vi har en fil som heter `user.json` med följande innehåll:
```json
{
	"name": "Johan",
	"email": "johan@go.com"
}
```

Kör programmet och du kommer att se utskriften:
```
Användarnamn: Johan
Email: johan@go.com
```

### Skrivning av JSON-data
För att skriva ut data till en JSON-fil i Go, kan vi använda samma `encoding/json` paket och dess funktion `Marshal()`. Här är ett exempel:

```Go
package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
)

type User struct {
	Name  string `json:"name"`
	Email string `json:"email"`
}

func main() {
	// Skapa en användare
	user := User{
		Name:  "Sara",
		Email: "sara@go.com",
	}

	// Använd Marshal() för att omvandla data till JSON-format
	jsonData, err := json.Marshal(user)
	if err != nil {
		fmt.Println(err.Error())
		return
	}

	// Skriv data till en fil
	err = ioutil.WriteFile("output.json", jsonData, 0644)
	if err != nil {
		fmt.Println(err.Error())
		return
	}

	fmt.Println("JSON-filen har skapats!")
}
```
Här kommer en fil att skapas som heter `output.json` med följande innehåll:
```json
{
	"name": "Sara",
	"email": "sara@go.com"
}
```

## Djupdykning
Om du vill bli mer bekant med att arbeta med JSON i Go, kan du utforska ytterligare funktioner i paketet `encoding/json`. Du kan också kolla in följande resurser för mer information:

- [Go Language specification on JSON](https://golang.org/ref/spec#JSON)
- [Gophercises: Exercise for practicing Go](https://gophercises.com/exercises/json)
- [Go by example: Working with JSON](https://gobyexample.com/json)

## Se även
- [Officiell dokumentation för encoding/json i Go](https://golang.org/pkg/encoding/json/)
- [How to Encode and Decode JSON Data in Go](https://www.digitalocean.com/community/tutorials/how-to-encode-and-decode-json-data-in-go)
- [Working with JSON in Go: Tips and Tricks](https://blog.alexellis.io/golang-json-api-client/)