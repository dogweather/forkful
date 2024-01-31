---
title:                "Working with XML"
date:                  2024-01-25T03:39:53.081748-07:00
model:                 gpt-4-1106-preview
simple_title:         "Working with XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/working-with-xml.md"
---

{{< edit_this_page >}}

## What & Why?
Working with XML involves parsing, creating, and manipulating XML documents using code. Programmers do it for data interchange, config files, and web services because XML’s readability and widespread support make it a solid choice for structured data.

## How to:
In Go, use the `encoding/xml` package. Let's parse and generate XML.
```go
package main

import (
	"encoding/xml"
	"fmt"
	"os"
)

// Structs map to XML elements
type Plant struct {
	XMLName xml.Name `xml:"plant"`
	Id      int      `xml:"id,attr"`
	Name    string   `xml:"name"`
	Origin  []string `xml:"origin"`
}

func main() {
	coffee := &Plant{Id: 27, Name: "Coffee"}
	coffee.Origin = []string{"Ethiopia", "Brazil"}

	// Marshal struct to XML
	output, err := xml.MarshalIndent(coffee, " ", "  ")
	if err != nil {
		fmt.Printf("Error: %v\n", err)
	}

	os.Stdout.Write([]byte(xml.Header))
	os.Stdout.Write(output)

	// Unmarshal XML to struct
	data := `
<plant id="27">
  <name>Coffee</name>
  <origin>Ethiopia</origin>
  <origin>Brazil</origin>
</plant>
`
	var p Plant
	if err := xml.Unmarshal([]byte(data), &p); err != nil {
		fmt.Printf("Error: %v", err)
		return
	}

	fmt.Printf("\n\nUnmarshaled: %+v", p)
}
```
Sample Output:
```xml
<?xml version="1.0" encoding="UTF-8"?>
 <plant id="27">
   <name>Coffee</name>
   <origin>Ethiopia</origin>
   <origin>Brazil</origin>
 </plant>

Unmarshaled: {XMLName:{Space: Local:plant} Id:27 Name:Coffee Origin:[Ethiopia Brazil]}
```

## Deep Dive
XML has been around since the late '90s, designed for large-scale electronic publishing but quickly adopted for the web. Alternatives like JSON have risen, touted for simplicity, but XML’s document validation through schemas and namespaces remain powerful for complex documents. In Go, `encoding/xml` handles most tasks, but for huge documents or stream processing, consider `xml.NewDecoder` and `xml.NewEncoder` for lower-level control and better performance.

## See Also
- Go's `encoding/xml` package: https://pkg.go.dev/encoding/xml
- XML tutorial: https://www.w3schools.com/xml/
- Go blog on XML: https://blog.golang.org/xml
- Comparison between JSON and XML: https://www.json.org/xml.html
