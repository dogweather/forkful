---
title:                "Trabalhando com json"
html_title:           "Go: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/working-with-json.md"
---

{{< edit_this_page >}}

## O que e por que?
JSON, ou JavaScript Object Notation, e um formato de dados popular usado por programadores para armazenar e transmitir informações estruturadas de forma legível e compacta. Isso facilita a comunicação entre diferentes sistemas e linguagens de programação.

## Como fazer:
Trabalhar com JSON em Go e bastante simples, pois a linguagem possui uma biblioteca padrão poderosa para manipulação de dados JSON. Veja um exemplo de como criar um JSON a partir de uma estrutura em Go:

```
type Pessoa struct {
	Nome string `json:"nome"`
	Email string `json:"email"`
	Telefone string `json:"telefone"`
}

p := Pessoa{"João", "joao@example.com", "(11) 99999-9999"}

jsonBytes, _ := json.Marshal(p) // Converte a estrutura para JSON
fmt.Println(string(jsonBytes))  // Exibe o JSON

```
Output: `{"nome":"João","email":"joao@example.com","telefone":"(11) 99999-9999"}`

Para ler um JSON e converter para uma estrutura em Go, podemos utilizar o seguinte código:

```
jsonString := `{"nome":"Maria","email":"maria@example.com","telefone":"(11) 88888-8888"}`

var p Pessoa
json.Unmarshal([]byte(jsonString), &p) // Converte o JSON para uma estrutura em Go
fmt.Println(p.Nome) // Exibe o nome da pessoa lido do JSON
```
Output: `Maria`

## Mergulho Profundo:
JSON foi originalmente criado por Douglas Crockford em 2001 e se tornou popular como um meio para enviar e receber dados pela web. Apesar de ser baseado em JavaScript, é uma linguagem independente e pode ser usado com vários tipos de aplicativos e linguagens de programação, incluindo Go.

Existem algumas alternativas populares ao uso de JSON, como XML e YAML. No entanto, JSON tem se mostrado mais leve, fácil de ler e de usar, tornando-se a escolha preferida para muitos desenvolvedores.

Em Go, a biblioteca padrão `encoding/json` possui funcionalidades avançadas, como a capacidade de trabalhar com dados JSON aninhados, apelidos de campos e tags de estrutura para personalizar o formato do JSON.

## Ver também:
- [Documentação oficial do pacote `encoding/json`](https://golang.org/pkg/encoding/json/)