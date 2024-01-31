---
title:                "Excluindo caracteres que correspondem a um padrão"
date:                  2024-01-20T17:42:29.623666-07:00
model:                 gpt-4-1106-preview
simple_title:         "Excluindo caracteres que correspondem a um padrão"

category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O Quê & Porquê?
Deletar caracteres que combinam com um padrão é remover partes específicas de uma string que atendem a uma regra definida, como todos os dígitos ou espaços. Programadores fazem isso para limpar dados, manipular texto, ou preparar informações para processamento.

## Como Fazer:
```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	// Exemplo 1: Removendo dígitos de uma string
	str := "A versão é 3.5.1"
	re := regexp.MustCompile(`\d`)
	strSemDigitos := re.ReplaceAllString(str, "")
	fmt.Println(strSemDigitos) // Saída: A versão é ...

	// Exemplo 2: Removendo espaços
	str2 := "Muito espaço aqui!"
	re2 := regexp.MustCompile(`\s`)
	strSemEspacos := re2.ReplaceAllString(str2, "")
	fmt.Println(strSemEspacos) // Saída: Muitoespaçoaqui!
}
```

## Aprofundando
Remover caracteres que correspondem a um padrão vem da necessidade de tratar strings de forma eficiente. Essa prática existe desde os primórdios da programação. 

Em Go, geralmente utilizamos o pacote `regexp` que implementa expressões regulares. Existem alternativas como iterar sobre caracteres e construir uma nova string, mas expressões regulares são poderosas e concisas.

Importante: o uso de `regexp` pode ser pesado em termos de desempenho. Em contextos onde a performance é crítica e o padrão é simples, métodos próprios de strings, como `strings.Replace`, podem ser mais rápidos.

## Veja Também

- Documentação oficial do pacote `regexp`: https://pkg.go.dev/regexp
- Tutorial Go by Example em strings: https://gobyexample.com/strings
- Artigo sobre performance de expressões regulares em Go: https://medium.com/@DylanMeeus/regex-performance-in-go-a-benchmark-648fca5983f2
