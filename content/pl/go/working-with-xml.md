---
title:                "Praca z XML"
date:                  2024-01-26T04:31:53.265159-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z XML"

category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/working-with-xml.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Praca z XML obejmuje analizowanie składni, tworzenie i manipulowanie dokumentami XML za pomocą kodu. Programiści robią to w celu wymiany danych, plików konfiguracyjnych oraz usług sieciowych, ponieważ czytelność XML i szerokie wsparcie czynią go solidnym wyborem dla strukturyzowanych danych.

## Jak to zrobić:
W Go, użyj pakietu `encoding/xml`. Przeanalizujmy i wygenerujmy XML.
```go
package main

import (
	"encoding/xml"
	"fmt"
	"os"
)

// Struktury mapowane do elementów XML
type Plant struct {
	XMLName xml.Name `xml:"plant"`
	Id      int      `xml:"id,attr"`
	Name    string   `xml:"name"`
	Origin  []string `xml:"origin"`
}

func main() {
	coffee := &Plant{Id: 27, Name: "Coffee"}
	coffee.Origin = []string{"Etiopia", "Brazylia"}

	// Marshalowanie struktury do XML
	output, err := xml.MarshalIndent(coffee, " ", "  ")
	if err != nil {
		fmt.Printf("Błąd: %v\n", err)
	}

	os.Stdout.Write([]byte(xml.Header))
	os.Stdout.Write(output)

	// Unmarshalowanie XML do struktury
	data := `
<plant id="27">
  <name>Coffee</name>
  <origin>Etiopia</origin>
  <origin>Brazylia</origin>
</plant>
`
	var p Plant
	if err := xml.Unmarshal([]byte(data), &p); err != nil {
		fmt.Printf("Błąd: %v", err)
		return
	}

	fmt.Printf("\n\nPrzekształcone: %+v", p)
}
```
Przykładowe wyjście:
```xml
<?xml version="1.0" encoding="UTF-8"?>
 <plant id="27">
   <name>Coffee</name>
   <origin>Etiopia</origin>
   <origin>Brazylia</origin>
 </plant>

Przekształcone: {XMLName:{Space: Local:plant} Id:27 Name:Coffee Origin:[Etiopia Brazylia]}
```

## Pogłębiona analiza
XML istnieje od końca lat 90., zaprojektowany dla dużych elektronicznych publikacji, ale szybko został przyjęty dla sieci. Alternatywy, takie jak JSON, zyskały na popularności z uwagi na prostotę, jednak walidacja dokumentów XML za pomocą schematów i przestrzeni nazw pozostaje potężnym narzędziem dla skomplikowanych dokumentów. W Go, `encoding/xml` radzi sobie z większością zadań, ale dla ogromnych dokumentów lub przetwarzania strumieniowego, rozważ użycie `xml.NewDecoder` i `xml.NewEncoder` do kontroli na niższym poziomie i lepszej wydajności.

## Zobacz także
- Pakiet `encoding/xml` w Go: https://pkg.go.dev/encoding/xml
- Samouczek XML: https://www.w3schools.com/xml/
- Blog Go na temat XML: https://blog.golang.org/xml
- Porównanie między JSON a XML: https://www.json.org/xml.html
