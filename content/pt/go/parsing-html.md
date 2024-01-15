---
title:                "Analisando html"
html_title:           "Go: Analisando html"
simple_title:         "Analisando html"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/parsing-html.md"
---

{{< edit_this_page >}}

## Por que

Você já precisou extrair informações de uma página da web? Se sim, você sabe o quão cansativo e demorado pode ser fazer isso manualmente. É aí que entra a análise HTML. Com o Go, é possível criar um programa que faça isso de forma eficiente e rápida.

## Como Fazer

Para começar a analisar o HTML com Go, é preciso importar a biblioteca "html" do pacote padrão. Em seguida, basta usar a função "Parse" passando o HTML como argumento. Veja um exemplo:

```Go
package main
import (
  "fmt"
  "html"
)

func main() {
  // HTML de exemplo
  const htmlString = "<h1>Título</h1><p>Parágrafo</p>" 

  // Analisa o HTML e armazena em uma variável
  parsedHTML := html.Parse(htmlString)

  // Imprime o resultado
  fmt.Println(parsedHTML)
}
```

O resultado será a estrutura do HTML analisada e pronta para ser utilizada em outras operações.

## Deep Dive

A biblioteca "html" do pacote padrão do Go oferece uma variedade de funções e métodos para facilitar a análise de HTML. Algumas delas incluem:

- `Parse`: como visto anteriormente, esta função analisa o HTML e retorna uma estrutura de dados.
- `NewTokenizer`: esta função cria um analisador HTML que pode ser usado para percorrer o HTML de forma mais granular.
- `Token`: a estrutura que representa um elemento do HTML. Possui métodos úteis para acessar seus atributos e conteúdo.
- `Render`: esta função renderiza o HTML analisado em um formato de string, permitindo a manipulação e edição antes de ser exportado.

A análise HTML com Go também permite o uso de seletores CSS, permitindo uma forma mais eficiente de encontrar e extrair informações específicas de uma página da web.

## Veja Também

- [Documentação Oficial do Pacote HTML do Go](https://golang.org/pkg/html/)
- [Tutorial de Análise de HTML com Go da Sitepoint (em inglês)](https://www.sitepoint.com/parsing-html-using-go/)