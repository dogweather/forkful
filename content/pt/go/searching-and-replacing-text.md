---
title:                "Pesquisando e substituindo texto"
html_title:           "Bash: Pesquisando e substituindo texto"
simple_title:         "Pesquisando e substituindo texto"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O Quê & Por quê?

Pesquisar e substituir texto é um processo de encontrar determinadas palavras ou sequências de texto em um documento ou código e substituí-las por outras. Os programadores fazem isso para corrigir erros, melhorar a clareza, ou alterar seu comportamento.

## Como fazer:

Aqui está um exemplo básico de como pesquisar e substituir texto em Go:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	oldText := "Olá, Mundo!"
	newText := strings.Replace(oldText, "Mundo", "Go", -1)
	fmt.Println(newText)
}
```

A saída desse código será `Olá, Go!`.

## Para mais detalhes:

Pesquisar e substituir texto tem sido uma função essencial na programação desde os primeiros text editors na década de 1980. Em Go, a função `strings.Replace` é muito útil. Mas, se quiser substituir texto com expressões regulares, você pode usar o pacote `regexp` também.

Além disso, a função `strings.Replace` retorna uma nova string, e não altera a string original. Isso ocorre porque, em Go, as strings são imutáveis.

## Veja também:

- Documentação oficial do pacote "strings" em Go: https://golang.org/pkg/strings/
- Tutorial sobre expressões regulares em Go (em inglês): https://gobyexample.com/regular-expressions

Com isso em mãos, você está pronto para pesquisar e substituir texto em Go!