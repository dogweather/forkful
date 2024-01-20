---
title:                "Utilizando expressões regulares"
html_title:           "Bash: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O Que São & Por Que Usar?
Regular expressions são padrões usados para encontrar correspondências dentro de strings. Programadores as utilizam para validar, extrair ou substituir texto de forma rápida e eficiente.

## Como Fazer:

```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	// Compilar a expressão regular
	r, _ := regexp.Compile("p([a-z]+)ch")

	// Testar se a expressão corresponde
	fmt.Println(r.MatchString("peach")) // Output: true

	// Encontrar a primeira correspondência
	fmt.Println(r.FindString("peach punch")) // Output: peach

	// Encontrar todas as correspondências com limite
	fmt.Println(r.FindAllString("peach punch pinch", -1)) // Output: [peach punch pinch]

	// Substituir texto correspondente
	fmt.Println(r.ReplaceAllString("a peach", "an apple")) // Output: an apple
}
```

## Mergulho Profundo:

As expressões regulares surgiram nos anos 1950 com teoria de autômatos e linguagem formal. Hoje, linguagens modernas como Go oferecem bibliotecas poderosas para regex. Alternativas incluem bibliotecas de análise sintática que são mais apropriadas para processar linguagens complexas. Em Go, regex é implementado no pacote `regexp`, que compila expressões para um formulário interno antes da execução, o que melhora a performance.

## Veja Também:
- Documentação oficial do pacote `regexp`: [Package regexp](https://pkg.go.dev/regexp)
- Tutorial interativo de regex: [RegexOne](https://regexone.com/)
- Ferramenta online para testar expressões regulares: [RegExr](https://regexr.com/)