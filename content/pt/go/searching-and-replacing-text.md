---
title:    "Go: Busca e substituição de texto"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Por que

A substituição de texto é uma tarefa comum na programação, e pode ser útil em diversas situações. Por exemplo, quando se quer corrigir erros ortográficos ou padronizar um trecho de código em várias linhas diferentes. Neste blog post, aprenderemos como realizar essa tarefa utilizando a linguagem de programação Go.

## Como fazer

A substituição de texto pode ser feita de forma simples utilizando a função `strings.Replace()` do pacote `strings`. Vamos ver um exemplo de como usar essa função através de um código em Go:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    texto := "Olá, mundo!"
    novoTexto := strings.Replace(texto, "Olá", "Oi", 1)
    fmt.Println(novoTexto)
}
```

Neste exemplo, estamos substituindo a palavra "Olá" por "Oi" no texto "Olá, mundo!". Ao executar esse código, o resultado será "Oi, mundo!". Além disso, podemos também especificar um terceiro argumento na função `Replace()` para indicar o número de substituições a serem feitas. Experimente alterar esse valor e ver como o resultado muda.

Outra forma de realizar a substituição de texto é utilizando expressões regulares. No Go, podemos fazer isso através do pacote `regexp`. Veja um exemplo:

```Go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    texto := "Este é um exemplo de texto com números: 12345"
    novoTexto := regexp.MustCompile("[0-9]+").ReplaceAllString(texto, "XXXXX")
    fmt.Println(novoTexto)
}
```

Neste caso, estamos substituindo todos os números do texto por "XXXXX". O resultado seria "Este é um exemplo de texto com números: XXXXX".

## Análise detalhada

A função `strings.Replace()` realiza a substituição de forma simples, mas é importante entender como ela funciona por baixo dos panos. Ela recebe quatro argumentos: o texto original, a palavra ou expressão a ser substituída, a nova palavra ou expressão e o número de substituições a serem feitas. No exemplo que vimos anteriormente, como atribuímos o valor 1 a esse quarto argumento, apenas a primeira ocorrência da palavra "Olá" foi substituída. Se o valor for -1 ou não for especificado, todas as ocorrências serão substituídas.

Já o uso de expressões regulares nos permite uma maior flexibilidade na substituição de texto, especialmente em casos mais complexos. A função `regexp.MustCompile()` compila uma expressão regular e a função `ReplaceAllString()` realiza a substituição em todo o texto.

## Veja também

- [Documentação oficial sobre a função `strings.Replace()`](https://golang.org/pkg/strings/#Replace)
- [Documentação oficial sobre o pacote `regexp`](https://golang.org/pkg/regexp/)
- [Tutorial sobre expressões regulares em Go](https://www.digitalocean.com/community/tutorials/how-to-use-regular-expressions-in-go-pt)
- [Exemplos de expressões regulares em Go](https://github.com/cnych/go-regexp-examples)