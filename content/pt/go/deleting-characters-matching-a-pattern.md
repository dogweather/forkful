---
title:                "Go: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que
Existe um recurso em linguagens de programação que permite que você exclua caracteres que correspondem a um determinado padrão. Isso pode ser útil em várias situações, como limpar dados, filtrar informações ou simplificar o código. Neste artigo, vamos explorar como fazer isso em Go, uma linguagem de programação que vem ganhando cada vez mais popularidade.

## Como fazer
Para excluir caracteres que correspondem a um padrão em Go, podemos usar a função `Delete` da biblioteca `strings`. Essa função recebe duas strings como parâmetros - a primeira é a string original e a segunda é o padrão que deve ser excluído. Por exemplo:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "Hello, world!"
	newStr := strings.Delete(str, "l")
	fmt.Println(newStr)
}
```

A saída desse código será "Heo, word!" - o caractere "l" foi excluído da string original. Também é possível passar um padrão mais complexo para a função, por exemplo, "ab" excluirá todas as ocorrências das letras "a" e "b" na string.

## Deep Dive
Essa função faz parte da biblioteca `strings` que possui muitas outras funções úteis para manipulação de strings. A função `Delete` é bastante simples, mas pode economizar muito tempo e esforço em tarefas de limpeza e filtragem de dados. Além disso, é interessante notar que ela não modifica a string original, ao invés disso, retorna uma nova string com as alterações feitas.

## Veja também
- [Documentação oficial do pacote strings em Go](https://golang.org/pkg/strings/)
- [Tutorial sobre a biblioteca strings em Go](https://www.geeksforgeeks.org/golang-strings-package/)
- [Exemplo prático de uso da função Delete em Go](https://www.calhoun.io/strategies-for-efficient-string-concatenation-in-go/)