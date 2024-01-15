---
title:                "Trabalhando com json."
html_title:           "Go: Trabalhando com json."
simple_title:         "Trabalhando com json."
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/working-with-json.md"
---

{{< edit_this_page >}}

## Por que trabalhar com JSON em Go?

JSON (JavaScript Object Notation) é um formato popular para troca de dados entre sistemas, sendo amplamente utilizado na web e em aplicações móveis. Ao trabalhar com Go, uma linguagem de programação de alto desempenho e eficiente em termos de recursos, trabalhar com JSON pode ajudar a tornar seu código mais flexível e interoperável.

## Como fazer

Para começar a trabalhar com JSON em Go, é necessário primeiro importar o pacote padrão `encoding/json`. Em seguida, podemos criar uma estrutura de dados que represente o formato JSON com o qual desejamos trabalhar.

```
package main

import (
	"encoding/json"
	"fmt"
	"log"
)

type Person struct {
	Name    string `json:"name"`
	Age     int    `json:"age"`
	Country string `json:"country"`
}

func main() {
	// Criando a estrutura de dados Person
	p := Person{
		Name:    "Maria",
		Age:     35,
		Country: "Brasil",
	}

	// Convertendo a estrutura em JSON
	jsonData, err := json.Marshal(p)
	if err != nil {
		log.Fatal(err)
	}

	// Imprimindo o JSON na tela
	fmt.Println(string(jsonData))
}
```

A saída deste código seria: `{"name":"Maria","age":35,"country":"Brasil"}`, que é um objeto JSON válido. Ao definir as tags `json` em nossa estrutura de dados, podemos controlar como os campos serão formatados no JSON resultante.

Mas e se quisermos fazer o inverso? Ou seja, converter um objeto JSON em uma estrutura de dados Go? Podemos fazer isso usando a função `json.Unmarshal()`.

```
// JSON de exemplo
jsonStr := `{"name":"João","age":40,"country":"Portugal"}`

// Convertendo o JSON para a estrutura Person
var p Person
err := json.Unmarshal([]byte(jsonStr), &p)
if err != nil {
	log.Fatal(err)
}

// Imprimindo os valores da estrutura
fmt.Println(p.Name)     // Saída: João
fmt.Println(p.Age)      // Saída: 40
fmt.Println(p.Country)  // Saída: Portugal
```

## Mergulho Profundo

Além das funções básicas de conversão de dados, Go também possui recursos poderosos para trabalhar com JSON, como a possibilidade de fazer a manipulação diretamente em streams de dados. Algumas outras coisas que vale a pena mencionar ao trabalhar com JSON em Go são:

- O pacote `encoding/json` também possui a função `NewDecoder()` que nos permite decodificar dados JSON de uma fonte de entrada, como um arquivo ou uma requisição HTTP.
- Para ignorar campos em branco durante a codificação do JSON, podemos usar a tag `json:"-"` em nossa estrutura de dados.
- Para definir campos opcionais em nossa estrutura de dados, podemos usar a tag `json:"nome_do_campo,omitempty"`, indicando que esse campo deve ser ignorado se estiver vazio.

## Veja também

- Documentação oficial sobre o pacote `encoding/json`: https://golang.org/pkg/encoding/json/
- Exemplos de uso do pacote `encoding/json`: https://gobyexample.com/json
- Tutorial completo sobre como trabalhar com JSON em Go: https://tutorialedge.net/golang/go-json-tutorial/