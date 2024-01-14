---
title:    "Go: Extraindo Substrings"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Por que extrair substrings em Go?

Extrair substrings de uma string maior pode ser útil em várias situações de programação. Por exemplo, pode ser necessário analisar um texto ou manipular um pequeno trecho de uma string. Em Go, podemos usar a função `Substr` para extrair facilmente substrings de uma string.

## Como fazer:

```Go
package main

import "fmt"

func main() {
    str := "Exemplo de uma string"
    
    // Extrai uma substring começando do índice 8 até o final
    fmt.Println(str[8:])
    
    // Extrai uma substring do índice 3 até o 7 (exclusivo)
    fmt.Println(str[3:7])
    
    // Extrai uma substring do índice 0 até o 2 (exclusivo)
    fmt.Println(str[:2])
}
```

**Saída:**

```
uma string
emp
Ex
```

Podemos ver que, ao usar a função `Substr` em Go, precisamos especificar o índice de início e, opcionalmente, o índice de fim da substring que desejamos extrair. Se não especificarmos o índice de fim, a substring será extraída até o final da string original.

## Mergulho profundo:

Ao trabalhar com strings em Go, é importante entender como o índice funciona. Em Go, os índices de uma string começam em 0 e vão até o comprimento da string - 1. Isso significa que, se tivermos uma string com o comprimento 10, o último índice seria 9.

Além disso, podemos usar índices negativos para extrair substrings a partir do final da string. Por exemplo, se usarmos o índice `-1`, estaremos acessando o último caractere da string, `-2` seria o penúltimo caractere e assim por diante.

Outra função útil para extrair substrings em Go é `SubstrIndex`, que nos permite encontrar o primeiro ou último índice de uma determinada substring em uma string maior. Isso pode ser útil para casos em que não conhecemos o índice exato de onde queremos extrair a substring.

## Veja também:

- [Documentação oficial do pacote strings](https://golang.org/pkg/strings/)
- [Guia de referência rápida do Go](https://github.com/a8m/go-lang-cheat-sheet/blob/master/calling_functions/channels/README.md#calling-functions)
- [Tutorial de strings em Go](https://www.golang-book.com/books/intro/8#section4)