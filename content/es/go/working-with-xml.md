---
title:                "Trabajando con XML"
date:                  2024-01-26T04:31:18.548580-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/working-with-xml.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Trabajar con XML implica analizar, crear y manipular documentos XML mediante código. Los programadores lo hacen para el intercambio de datos, archivos de configuración y servicios web porque la legibilidad de XML y su amplio soporte lo convierten en una elección sólida para datos estructurados.

## Cómo hacerlo:
En Go, utiliza el paquete `encoding/xml`. Vamos a analizar y generar XML.
```go
package main

import (
	"encoding/xml"
	"fmt"
	"os"
)

// Las estructuras se mapean a elementos XML
type Plant struct {
	XMLName xml.Name `xml:"plant"`
	Id      int      `xml:"id,attr"`
	Name    string   `xml:"name"`
	Origin  []string `xml:"origin"`
}

func main() {
	coffee := &Plant{Id: 27, Name: "Coffee"}
	coffee.Origin = []string{"Etiopía", "Brasil"}

	// Marcar estructura como XML
	output, err := xml.MarshalIndent(coffee, " ", "  ")
	if err != nil {
		fmt.Printf("Error: %v\n", err)
	}

	os.Stdout.Write([]byte(xml.Header))
	os.Stdout.Write(output)

	// Desmarcar XML a estructura
	data := `
<plant id="27">
  <name>Coffee</name>
  <origin>Etiopía</origin>
  <origin>Brasil</origin>
</plant>
`
	var p Plant
	if err := xml.Unmarshal([]byte(data), &p); err != nil {
		fmt.Printf("Error: %v", err)
		return
	}

	fmt.Printf("\n\nDesmarcado: %+v", p)
}
```
Salida de muestra:
```xml
<?xml version="1.0" encoding="UTF-8"?>
 <plant id="27">
   <name>Coffee</name>
   <origin>Etiopía</origin>
   <origin>Brasil</origin>
 </plant>

Desmarcado: {XMLName:{Space: Local:planta} Id:27 Name:Coffee Origin:[Etiopía Brasil]}
```

## Análisis Profundo
XML existe desde finales de los '90, diseñado para la publicación electrónica a gran escala pero rápidamente adoptado para la web. Alternativas como JSON han surgido, alabadas por su simplicidad, pero la validación de documentos de XML a través de esquemas y espacios de nombres sigue siendo poderosa para documentos complejos. En Go, `encoding/xml` maneja la mayoría de las tareas, pero para documentos enormes o procesamiento de flujo, considera `xml.NewDecoder` y `xml.NewEncoder` para un control de bajo nivel y un mejor rendimiento.

## Véase También
- Paquete `encoding/xml` de Go: https://pkg.go.dev/encoding/xml
- Tutorial de XML: https://www.w3schools.com/xml/
- Blog de Go sobre XML: https://blog.golang.org/xml
- Comparación entre JSON y XML: https://www.json.org/xml.html
