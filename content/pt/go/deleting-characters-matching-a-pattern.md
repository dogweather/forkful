---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Arduino: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O que e Por que?

Deletar caracteres que correspondem a um padrão é uma atividade comum no desenvolvimento de software. Programadores fazem isso para limpar dados, remover caracteres indesejados ou anticuados e normalizar strings.

## Como fazer:

No Go, costumamos usar o pacote "strings" para manipular strings. Aqui vai um exemplo de como você pode deletar caracteres que correspondem a um padrão:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "Hello, person! How are you today?"
	str = strings.ReplaceAll(str, " ", "")
	fmt.Println(str)
}
```

Resultado de saída:

```Go
Hello,person!Howareyoutoday?
```

No código acima, todos os espaços em branco na string foram removidos.

## Aprofundando:

Historicamente, a eliminação de caracteres seguindo um padrão tem sido uma prática comum em muitos outros idiomas também. Em linguagens como Python ou JavaScript, funções de substituição de strings também são amplamente utilizadas para esses propósitos.

Existem várias outras maneiras de abordar tal problema em Go. Alternativamente, você pode usar o pacote "regexp" para corresponder a um padrão em um texto e remover ou substituir:

```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	str := "Hello, person! How are you today? How about tomorrow?"
	reg := regexp.MustCompile(`(today|tomorrow)`)
	str = reg.ReplaceAllString(str, "")
	fmt.Println(str)
}
```

## Veja também:

1. [Pacote oficial de strings Go](https://golang.org/pkg/strings/)
2. [Golang, expressões regulares e o pacote regexp](https://gobyexample.com/regular-expressions)
3. [Go: uma visão geral das strings](https://go.dev/blog/strings)