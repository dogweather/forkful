---
title:                "Descobrindo o comprimento de uma string"
date:                  2024-01-20T17:47:46.671158-07:00
model:                 gpt-4-1106-preview
simple_title:         "Descobrindo o comprimento de uma string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Calcular o tamanho de uma string significa contar o número de caracteres que ela possui. Programadores fazem isso para validar entrada de dados, delimitar texto, ou simplesmente como parte da lógica de manipulação de strings.

## How to:
Em Go, usamos a função `len()` para obter o comprimento de uma string. Confira o exemplo:

```Go
package main

import "fmt"

func main() {
    greeting := "Olá, mundo!"
    length := len(greeting)
    fmt.Printf("A string '%s' tem %d caracteres.\n", greeting, length)
}
```

Output esperado:

```
A string 'Olá, mundo!' tem 12 caracteres.
```

Note que, se a string contiver caracteres especiais (ou seja, não ASCII), o resultado pode não ser o esperado, pois `len()` conta bytes, não caracteres individuais ou runas.

## Deep Dive
Em Go, strings são fatias de bytes imutáveis. Há situações em que calcular o comprimento de uma string pode ficar complicado, especialmente quando trabalhamos com caracteres Unicode que podem ocupar mais de um byte. O termo para um caractere Unicode é "rune" e, se você precisar do número correto de runas, use a função `utf8.RuneCountInString()` do pacote `unicode/utf8`.

Alternativas incluem a iteração manual sobre a string com um loop `for range`, contando runas individualmente ou usando bibliotecas de terceiros que fornecem mais funcionalidades para manipulação de strings.

Detalhes da implementação, como o algoritmo de contagem de caracteres ou a representação interna de strings no Go, podem não ser relevantes para a tarefa diária de pegar o comprimento de uma string. Contudo, eles são cruciais quando a performance é uma preocupação ou quando trabalhamos com conjuntos complexos de caracteres.

## See Also
- Documentação oficial do Go para strings: https://golang.org/pkg/strings/
- Pacote unicode/utf8: https://golang.org/pkg/unicode/utf8/
- The Go Blog - Strings, bytes, runes and characters in Go: https://blog.golang.org/strings
