---
title:                "Werken met JSON"
date:                  2024-01-28T22:10:33.252305-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met JSON"

category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/working-with-json.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Werken met JSON betekent het coderen en decoderen van gegevens in het JavaScript Object Notation formaat, een op tekst gebaseerde manier om gestructureerde gegevens te vertegenwoordigen. Programmeurs gebruiken het vanwege de eenvoud en alomtegenwoordigheid in web-API's en configuratiebestanden.

## Hoe:

### JSON in Go Mappen

```Go
package main

import (
	"encoding/json"
	"fmt"
)

type Gebruiker struct {
	Naam   string `json:"name"`
	Leeftijd    int    `json:"age"`
	Actief bool   `json:"active"`
}

func main() {
	gebruiker := Gebruiker{Naam: "Alice", Leeftijd: 25, Actief: true}
	jsonData, err := json.Marshal(gebruiker)
	if err != nil {
		panic(err)
	}
	fmt.Println(string(jsonData))
}
```

Voorbeelduitvoer:
```json
{"name":"Alice","age":25,"active":true}
```

### JSON in Go Demappen

```Go
package main

import (
	"encoding/json"
	"fmt"
)

func main() {
	var jsonData = []byte(`{"name":"Alice","age":25,"active":true}`)
	gebruiker := Gebruiker{}
	err := json.Unmarshal(jsonData, &gebruiker)
	if err != nil {
		panic(err)
	}
	fmt.Printf("%+v\n", gebruiker)
}

type Gebruiker struct {
	Naam   string `json:"name"`
	Leeftijd    int    `json:"age"`
	Actief bool   `json:"active"`
}
```

Voorbeelduitvoer:
```
{Naam:Alice Leeftijd:25 Actief:true}
```

## Diepgaande Toelichting

JSON, ontstaan uit JavaScript, werd midden jaren 2000 een standaard voor gegevensuitwisseling. Vergeleken met XML is het lichter en leesbaar voor mensen, wat de reden is waarom het de voorkeur heeft voor RESTful API's. In Go behandelt het `encoding/json` pakket JSON-gegevens, en gebruikt het de tags van veldstructuren om JSON-sleutels met structuurvelden overeen te laten komen.

Alternatieven voor JSON zijn XML, YAML en binaire formaten zoals Protocol Buffers (protobuf). Elk heeft zijn gebruikscases; bijvoorbeeld, YAML wordt geprefereerd voor door mensen geschreven configuratiebestanden, terwijl protobuf wordt gebruikt voor efficiënte, platformneutrale gegevensuitwisseling.

Go implementeert de afhandeling van JSON efficiënt, hoewel het gebruik van reflectie ervoor kan zorgen dat het in vergelijking met sommige serialisatiemechanismen, die tijdens de compilatie kunnen werken, langzamer is.

## Zie Ook

- De Go Blog over JSON: https://blog.golang.org/json
- Go `encoding/json` pakketdocumentatie: https://pkg.go.dev/encoding/json
- Officiële site van JSON voor de standaard: http://json.org/
