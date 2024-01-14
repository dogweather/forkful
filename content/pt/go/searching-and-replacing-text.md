---
title:                "Go: Buscando e substituindo texto"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

A substituição de textos é uma tarefa comum e importante em programação. Ela permite que você faça alterações em um texto de forma eficiente e rápida, economizando tempo e esforço. Alguns exemplos de cenários em que a substituição de textos pode ser útil incluem correção de erros de digitação, padronização de formatos e modificação de informações específicas em uma grande quantidade de dados.

## Como fazer

Em Go, a substituição de textos é realizada por meio da função `strings.Replace`. Essa função aceita quatro argumentos: a string original, a string a ser substituída, a nova string e o número máximo de substituições a serem feitas. Veja um exemplo abaixo:

```Go
package main

import "fmt"
import "strings"

func main() {
	texto := "Olá, mundo!"
	novoTexto := strings.Replace(texto, "Olá", "Oi", 1)
	fmt.Println(novoTexto)
}
```

A saída desse código seria "Oi, mundo!", pois a função substituiu apenas a primeira ocorrência da palavra "Olá" por "Oi".

## Aprofundando

Além da função `Replace`, a biblioteca `strings` do Go também oferece outras maneiras de realizar a substituição de textos. Algumas delas incluem:

- `strings.ReplaceAll`: funciona da mesma forma que a função `Replace`, mas substitui todas as ocorrências da string buscada;
- `strings.ReplaceAllLiteral`: realiza a substituição sem interpretar nenhum caractere especial, como em uma busca por exatamente a string `"\n"` no texto;
- `strings.Map`: permite que você utilize uma função para mapear cada caractere da string e substituir os que forem desejados.

Vale lembrar que todas essas funções são sensíveis a maiúsculas e minúsculas, ou seja, "Olá" e "olá" são considerados strings diferentes durante a busca e substituição.

## Veja também

Para saber mais sobre a substituição de textos em Go, confira a documentação oficial da biblioteca `strings` (https://golang.org/pkg/strings/) e explore outras funções disponíveis.