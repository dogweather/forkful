---
title:                "Trabalhando com XML"
date:                  2024-01-26T04:31:28.254835-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com XML"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/working-with-xml.md"
---

{{< edit_this_page >}}

## O Quê & Porquê?
Trabalhar com XML envolve analisar, criar e manipular documentos XML usando código. Os programadores fazem isso para intercâmbio de dados, arquivos de configuração e serviços web porque a legibilidade do XML e o amplo suporte o tornam uma escolha sólida para dados estruturados.

## Como Fazer:
No Go, use o pacote `encoding/xml`. Vamos analisar e gerar XML.
```go
package main

import (
	"encoding/xml"
	"fmt"
	"os"
)

// Structs mapeiam para elementos XML
type Plant struct {
	XMLName xml.Name `xml:"plant"`
	Id      int      `xml:"id,attr"`
	Name    string   `xml:"name"`
	Origin  []string `xml:"origin"`
}

func main() {
	coffee := &Plant{Id: 27, Name: "Coffee"}
	coffee.Origin = []string{"Ethiopia", "Brazil"}

	// Marshal struct para XML
	output, err := xml.MarshalIndent(coffee, " ", "  ")
	if err != nil {
		fmt.Printf("Erro: %v\n", err)
	}

	os.Stdout.Write([]byte(xml.Header))
	os.Stdout.Write(output)

	// Unmarshal XML para struct
	data := `
<plant id="27">
  <name>Coffee</name>
  <origin>Etiópia</origin>
  <origin>Brasil</origin>
</plant>
`
	var p Plant
	if err := xml.Unmarshal([]byte(data), &p); err != nil {
		fmt.Printf("Erro: %v", err)
		return
	}

	fmt.Printf("\n\nDesmarcado: %+v", p)
}
```
Exemplo de Saída:
```xml
<?xml version="1.0" encoding="UTF-8"?>
 <plant id="27">
   <name>Coffee</name>
   <origin>Etiópia</origin>
   <origin>Brasil</origin>
 </plant>

Desmarcado: {XMLName:{Space: Local:plant} Id:27 Name:Coffee Origin:[Etiópia Brasil]}
```

## Aprofundamento
XML existe desde o final dos anos 90, projetado para publicação eletrônica em larga escala, mas rapidamente adotado para a web. Alternativas como JSON surgiram, elogiadas pela simplicidade, mas a validação de documentos do XML através de esquemas e namespaces permanecem poderosos para documentos complexos. No Go, o `encoding/xml` lida com a maioria das tarefas, mas para documentos enormes ou processamento de stream, considere `xml.NewDecoder` e `xml.NewEncoder` para um controle de nível mais baixo e melhor desempenho.

## Veja Também
- Pacote `encoding/xml` do Go: https://pkg.go.dev/encoding/xml
- Tutorial de XML: https://www.w3schools.com/xml/
- Blog do Go sobre XML: https://blog.golang.org/xml
- Comparação entre JSON e XML: https://www.json.org/xml.html
