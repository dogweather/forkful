---
title:    "Go: Capitalizando uma string"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string em Go?

Capitalizar uma string em Go é uma operação muito comum e útil em muitos cenários de programação. Ela pode ajudar a melhorar a legibilidade do código, a organizar e formatar dados e a garantir a consistência e correção dos dados manipulados pelo programa.

## Como capitalizar uma string em Go?

Existem algumas maneiras diferentes de capitalizar uma string em Go, dependendo do objetivo e do contexto do programa. Aqui estão alguns exemplos usando a função `strings.Title` e seu uso dentro de um loop:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    // Criando uma string
    str := "esta é uma string a ser capitalizada"

    // Usando a função strings.Title para capitalizar a string
    capitalized := strings.Title(str)

    // Imprimindo o resultado
    fmt.Println(capitalized) // Saída: Esta É Uma String A Ser Capitalizada

    // Criando um slice de strings
    words := []string{"maçã", "banana", "cereja"}

    // Usando um loop para capitalizar cada elemento do slice
    for i, word := range words {
        fmt.Println(strings.Title(word)) // Saída: Maçã, Banana, Cereja
        words[i] = strings.Title(word) // Substituindo os elementos do slice pelos valores capitalizados
    }

    // Imprimindo o slice após a capitalização
    fmt.Println(words) // Saída: [Maçã Banana Cereja]
}
```

## Deep Dive: Entendendo o processo de capitalização em Go

O processo de capitalização em Go é baseado na função `strings.Title`, que utiliza as regras Unicode para capitalizar a primeira letra de cada palavra em uma string. Além disso, essa função também irá respeitar letras maiúsculas já existentes em uma palavra. Por exemplo, se a string contém a palavra "iPhone", essa palavra será mantida como "iPhone" após a aplicação da função.

É importante mencionar que a capitalização de letras em Go é sensível a acentos e caracteres especiais, então é preciso estar atento a isso ao utilizar essa função em suas aplicações.

## Veja também

- [Documentação oficial do pacote strings em Go](https://golang.org/pkg/strings/)
- [Tutorial sobre strings em Go](https://gobyexample.com/strings)
- [Exemplos de capitalização de strings em Go](https://www.programming-books.io/essential/go/string-capitalize-e95fadf769504989bce31d468f161e62)