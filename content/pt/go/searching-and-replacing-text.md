---
title:    "Go: Busca e substituição de texto"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que fazer substituição de texto em Go

Ao escrever um programa em Go, pode ser necessário fazer substituições de texto para alterar ou corrigir dados. A capacidade de encontrar e substituir texto é essencial para a manipulação de strings. Felizmente, Go possui recursos poderosos para realizar essa tarefa de forma eficiente.

## Como fazer substituição de texto em Go

Para fazer substituições de texto em Go, usamos a função `strings.Replace()`. Esta função recebe quatro argumentos: a string original, a string a ser substituída, a nova string e o número de ocorrências a serem substituídas. Aqui está um exemplo de como usar essa função:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "Olá mundo!"
    newStr := strings.Replace(str, "mundo", "Go", 1)
    fmt.Println(newStr)
}
```

Neste exemplo, a função `strings.Replace()` substitui a palavra "mundo" por "Go" apenas uma vez na string "Olá mundo!". Portanto, o resultado impresso será "Olá Go!".

Outra maneira de fazer substituições de texto em Go é usar o pacote `regexp`. Esse pacote permite o uso de expressões regulares para encontrar e substituir padrões em uma string. Aqui está um exemplo:

```Go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    str := "A data de hoje é 20/01/2021."
    re := regexp.MustCompile(`(\d{2})/(\d{2})/(\d{4})`)
    newStr := re.ReplaceAllString(str, "$3-$2-$1")
    fmt.Println(newStr)
}
```

Neste exemplo, usamos a expressão regular `(\d{2})/(\d{2})/(\d{4})`, que procura um padrão de data na string. Em seguida, a função `ReplaceAllString()` substitui esse padrão pelo formato "AAAA-MM-DD". Portanto, o resultado será "A data de hoje é 2021-01-20.".

## Profundidade na substituição de texto em Go

Além das funções mencionadas, Go possui muitos outros recursos para manipulação de strings, como a função `strings.ReplaceAll()` que substitui todas as ocorrências de uma string, e o pacote `unicode` que permite o tratamento de caracteres Unicode em substituições de texto. É importante entender esses recursos e escolher a melhor opção para cada tarefa específica de substituição de texto.

## Veja também
- [Documentação oficial strings.Replace()](https://golang.org/pkg/strings/#Replace)
- [Tutorial sobre substituição de string em Go](https://www.calhoun.io/using-string-replace-in-go/)
- [Documentação oficial do pacote regexp](https://golang.org/pkg/regexp/)