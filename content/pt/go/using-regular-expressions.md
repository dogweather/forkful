---
title:                "Usando expressões regulares"
html_title:           "Go: Usando expressões regulares"
simple_title:         "Usando expressões regulares"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Regular expressions, ou expressões regulares, são padrões de texto usados para buscar e manipular valores em uma string. Programadores as utilizam para validar dados de entrada, fazer substituições em documentos e realizar buscas em grandes conjuntos de dados.

## Como fazer:
Veja abaixo alguns exemplos de como usar expressões regulares em Go:

```Go
// Encontre todas as palavras que começam com "go"
re := regexp.MustCompile(`go\S*`)
fmt.Println(re.FindAllString("go time, gophers!", -1))

// Resultado:
// [go time, gophers!]

// Substitua todas as ocorrências de "gopher" por "Golang"
re := regexp.MustCompile(`gopher`)
fmt.Println(re.ReplaceAllString("Gophers are amazing!", "Golang"))

// Resultado:
// Golangs are amazing!
```

## Deep Dive:
As expressões regulares foram inventadas em 1951 pelo matemático Stephen Cole Kleene para descrever linguagens formais. Elas são suportadas em muitas linguagens de programação, incluindo Go. Alternativas para expressões regulares incluem a função "strings.Contains" em Go e a biblioteca "parse" em Python. Para implementar expressões regulares em Go, a biblioteca "regexp" inclui funções para compilar, pesquisar e substituir padrões em strings.

## Veja também:
- [Documentação oficial do pacote "regexp" em Go](https://golang.org/pkg/regexp/)