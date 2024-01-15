---
title:                "Convertendo uma string para minúsculas"
html_title:           "Go: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que

Às vezes, precisamos converter uma string para letras minúsculas para fins de comparação ou formatação correta em nosso código Go. Isso pode ser útil ao trabalhar com entradas do usuário ou ao manipular dados de texto.

## Como Fazer

A conversão de uma string para letras minúsculas em Go é bastante simples. Basta usar a função `strings.ToLower ()` e passar a string como argumento. Veja um exemplo abaixo:

```Go
package main

import "fmt"
import "strings"

func main() {
	str := "OLA MUNDO"
	lower := strings.ToLower(str)

	fmt.Println(lower)
}
```

A saída seria "ola mundo", todos em letras minúsculas. Podemos também usar a função `strings.ToLower()` em uma linha, sem a necessidade de criar uma variável para armazenar o resultado:

```Go
lower := strings.ToLower("OLA MUNDO")
```

## Mergulho Profundo

Ao converter uma string para letras minúsculas em Go, é importante levar em conta que a conversão é baseada nos códigos de caracteres Unicode. Isso significa que, ao converter uma letra maiúscula para minúscula, o resultado pode ser diferente do que esperamos em alguns casos.

Por exemplo, a conversão da letra "İ" (I com ponto no topo) em minúscula resultará em "i̇" (i com ponto no meio). No entanto, se esperávamos apenas "i" minúsculo, pode ser necessário usar a função `strings.Replace()` para remover o ponto extra.

## Veja Também

- [Documentação oficial de strings.ToLower()](https://golang.org/pkg/strings/#ToLower)
- [Artigo sobre strings em Go](https://www.geeksforgeeks.org/golang-strings-package/)
- [Exemplos de código para prática em Go](https://tour.golang.org/list)