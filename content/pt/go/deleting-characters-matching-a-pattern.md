---
title:    "Go: Excluindo caracteres que correspondem a um padrão"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Às vezes, ao escrever códigos em Go, pode ser necessário remover caracteres que correspondam a um determinado padrão. Isso pode ser útil em situações como limpar dados de entrada ou correção de erros.

## Como Fazer

Deletar caracteres correspondentes a um padrão é fácil de fazer usando a biblioteca padrão de strings. A função `ReplaceAllString` pode ser usada para procurar e substituir todos os caracteres que correspondem ao padrão em uma string. Veja um exemplo de código abaixo:

```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	// Definindo a string original
	str := "Go é uma linguagem de programação incrível"

	// Definindo o padrão a ser procurado
	pattern := "a"

	// Criando uma expressão regular para o padrão
	reg := regexp.MustCompile(pattern)

	// Substituindo o padrão por uma string vazia,
	// o que essencialmente deleta o caracter
	newStr := reg.ReplaceAllString(str, "")

	// Imprimindo a string resultante
	fmt.Println(newStr)

	// Saída: Go é um lnggem de progrmção incrível
}
```

## Deep Dive

A função `ReplaceAllString` do pacote `regexp` procura e substitui todas as ocorrências de um padrão em uma string. Isso significa que, se houver mais de uma ocorrência do padrão na string, todas serão substituídas. Além disso, a função é sensível a maiúsculas e minúsculas, ou seja, se o padrão for "a" e a string contiver "A", ela também será substituída. É importante observar ainda que, ao usar esta função, não se pode especificar uma posição de início ou fim para a busca, ela sempre começa no início e termina no final da string.

## Veja Também

- Documentação oficial do pacote `regexp` em Go: https://golang.org/pkg/regexp/
- Artigo sobre expressões regulares em Go: https://blog.golang.org/regular-expressions
- Outro exemplo de uso da função `ReplaceAllString`: https://www.geeksforgeeks.org/golang-strings-replaceallstring-function/