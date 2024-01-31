---
title:                "Trabalhando com JSON"
date:                  2024-01-19
simple_title:         "Trabalhando com JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/working-with-json.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?
Trabalhar com JSON em Go é lidar com a codificação e decodificação de dados no formato JavaScript Object Notation, um padrão leve de troca de informações. Programadores fazem isso porque JSON é universalmente reconhecido, fácil de entender e utilizado em APIs e configurações.

## Como Fazer:
```Go
package main

import (
	"encoding/json"
	"fmt"
	"log"
)

// Estrutura para mapear o JSON
type Pessoa struct {
	Nome string `json:"nome"`
	Idade int `json:"idade"`
}

func main() {
	// Serializando: struct para JSON
	p := Pessoa{"Alice", 30}
	jsonData, err := json.Marshal(p)
	if err != nil {
		log.Fatalf("Erro ao converter para JSON: %v", err)
	}
	fmt.Println(string(jsonData))

	// Desserializando: JSON para struct
	var p2 Pessoa
	err = json.Unmarshal(jsonData, &p2)
	if err != nil {
		log.Fatalf("Erro ao ler JSON: %v", err)
	}
	fmt.Printf("%+v\n", p2)
}
```
Saída:
```
{"nome":"Alice","idade":30}
{Nome:Alice Idade:30}
```

## Aprofundando:
JSON surgiu em 2001, proposto por Douglas Crockford. Em Go, o pacote `encoding/json` possibilita trabalhar com JSON. Alternativas incluem XML e YAML, mas JSON destaca-se pela simplicidade. Implementações em Go usam struct tags para mapear campos do JSON para a struct, o que permite uma conversão direta e type-safe.

## Veja Também:
- Documentação oficial do Go para o pacote `encoding/json`: https://golang.org/pkg/encoding/json/
- Tutorial JSON em Go: https://www.sohamkamani.com/golang/json/
- Comparação entre JSON, XML e YAML: https://blog.logrocket.com/json-vs-xml-vs-yaml-comparison/
