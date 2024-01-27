---
title:                "Arbeid med JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeid med JSON"
simple_title:         "Arbeid med JSON"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/working-with-json.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
JSON (JavaScript Object Notation) er et datautvekslingsformat. Det er populært fordi det er lettleselig for både mennesker og maskiner, og det er språkuavhengig, så det fungerer godt for web APIs og konfigureringsfiler.

## Hvordan gjøres det:
```Go
package main

import (
	"encoding/json"
	"fmt"
)

type User struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
}

func main() {
	// JSON encoding
	user := User{"Ola Nordmann", 30}
	jsonData, err := json.Marshal(user)
	if err != nil {
		panic(err)
	}
	fmt.Println(string(jsonData))

	// JSON decoding
	var decodedUser User
	err = json.Unmarshal(jsonData, &decodedUser)
	if err != nil {
		panic(err)
	}
	fmt.Printf("%+v\n", decodedUser)
}
```
Output:
```
{"name":"Ola Nordmann","age":30}
{Name:Ola Nordmann Age:30}
```

## Dypdykk
JSON ble populær rundt 2000 som et alternativ til XML, fordi det er mindre verbose og enklere å parse. Alternativer til JSON inkluderer YAML, XML og ProtoBuf. I Go blir JSON bearbeidet ved hjelp av “encoding/json”-pakken. Pakken reflekterer over strukturene for å serialisere Go-verdier til JSON og omvendt, noe som noen ganger krever structs-tags for å styre oppførselen.

## Se også
- Go’s dokumentasjon om JSON: https://golang.org/pkg/encoding/json/
- JSON hjemmeside for generell forståelse: https://www.json.org/json-en.html
- Wikipedia-side om JSON for historisk kontekst: https://no.wikipedia.org/wiki/JSON
