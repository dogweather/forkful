---
title:                "Go: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que?

Encontrar o comprimento de uma string é uma tarefa básica na programação e pode ser útil em várias situações. Saber o tamanho de uma string pode ajudar a validar entradas de usuário, formatar a saída de dados ou até mesmo otimizar algoritmos de busca.

## Como fazer

Para encontrar o comprimento de uma string em Go, podemos usar a função `len ()` que retorna o número de bytes da string. Vamos dar uma olhada em alguns exemplos:

```
package main

import "fmt"

func main() {
    str1 := "Olá Mundo!"
    str2 := "😊🚀"

    fmt.Println(len(str1))
    fmt.Println(len(str2))
}
```

A saída desse código será:

```
10
4
```

Note que, como o Go é uma linguagem unicode, os emojis também são contados como um byte cada. Isso pode ser útil quando trabalhamos com caracteres unicode em nossos programas.

Também podemos usar a função `RuneCountInString ()` para contar o número de caracteres em uma string:

```
package main

import "fmt"
import "unicode/utf8"

func main() {
    str := "Olá Mundo!"

    fmt.Println(utf8.RuneCountInString(str))
}
```

A saída desse código será `10`, já que a função `RuneCountInString()` conta o número de caracteres e não de bytes.

## Mergulho Profundo

A função `len()` e `RuneCountInString()` são eficientes para encontrar o comprimento de uma string, mas elas podem não fornecer o resultado esperado em alguns casos. Por exemplo, elas não contam corretamente o número de caracteres se a string contém caracteres acentuados ou emojis compostos por mais de um caractere.

Para lidar com esse problema, podemos usar a biblioteca `utf8` do Go e a função `RuneCount()`, que conta o número de runas em uma string. Uma runa é um caractere unicode e é representado por um ou mais bytes.

```
package main

import "fmt"
import "unicode/utf8"

func main() {
    str := "Olá Mundo!"

    fmt.Println(utf8.RuneCount([]byte(str)))
}
```

A saída desse código será `10`, pois a função `RuneCount()` conta corretamente o número de caracteres unicode na string.

## Veja também
- [Documentação oficial do Go sobre strings](https://golang.org/pkg/strings/)
- [Tutorial sobre strings em Go](https://www.tutorialspoint.com/go/go_strings.htm)
- [Guia de referência rápida para manipulação de strings em Go](https://yourbasic.org/golang/string-functions-reference-cheat-sheet/)