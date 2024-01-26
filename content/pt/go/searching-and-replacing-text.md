---
title:                "Pesquisando e substituindo texto"
date:                  2024-01-20T17:58:06.974503-07:00
model:                 gpt-4-1106-preview
simple_title:         "Pesquisando e substituindo texto"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?
Buscar e substituir texto é uma técnica de manipulação de strings onde buscamos um padrão específico e trocamos por outro. Programadores fazem isso para actualizar dados, corrigir erros, ou reformatar informações de maneira eficiente.

## Como Fazer:
```go
package main

import (
	"fmt"
	"strings"
)

func main() {
	textoOriginal := "O Go é uma linguagem incrível."
	busca := "incrível"
	substituicao := "fantástica"

	textoModificado := strings.Replace(textoOriginal, busca, substituicao, -1)

	fmt.Println("Antes:", textoOriginal)
	fmt.Println("Depois:", textoModificado)
}

```
Saída de exemplo:
```
Antes: O Go é uma linguagem incrível.
Depois: O Go é uma linguagem fantástica.
```

## Mergulho Profundo
Historicamente, a capacidade de buscar e substituir texto é uma operação antiquíssima na computação, com referências até o tempo dos editores de texto como o vi. Em pequena escala, essa troca é simples. Mas em grandes volumes de dados ou textos mais complexos, consideramos expressões regulares para buscas mais sofisticadas. Alternativas incluem o uso de funções específicas para regex em Go, como `regexp.Compile` e `regexp.ReplaceAllString`. Em termos de implementação, a eficiência é chave, especialmente com buscas em grandes strings—assim, algoritmos de busca como o KMP (Knuth-Morris-Pratt) entram em cena para otimizar a procura por padrões.

## Veja Também

- [Pacote strings](https://golang.org/pkg/strings/)
- [Pacote regexp](https://golang.org/pkg/regexp/)
- [Documentação oficial do Go](https://golang.org/doc/)
- [Tutorial de Expressões Regulares em Go](https://gobyexample.com/regular-expressions)
