---
title:                "Go: Encontrando o comprimento de uma string"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Encontrar o comprimento de uma string é uma tarefa bastante comum ao programar em Go. É importante entender como essa função funciona e como utilizá-la corretamente.

## Como Fazer

Para encontrar o comprimento de uma string em Go, podemos utilizar a função `len()`. Esta função retorna um valor numérico correspondente ao número total de bytes da string.

```Go
texto := "Olá, mundo!"
fmt.Println(len(texto)) // Output: 12
```

Note que em Go, cada caractere de uma string é representado por um byte. Portanto, o comprimento de uma string pode ser diferente do número de caracteres individuais.

## Mergulho Profundo

Além da função `len()`, também podemos utilizar o pacote `unicode/utf8` para encontrar o número de caracteres em uma string. Este pacote possui a função `RuneCountInString()` que retorna o número de caracteres Unicode presentes na string.

```Go
import (
    "fmt"
    "unicode/utf8"
)

texto := "Olá, mundo!"
fmt.Println(utf8.RuneCountInString(texto)) // Output: 10
```

Isso acontece porque a letra "á" em uma string é representada por dois bytes em UTF-8, enquanto que em Unicode, é representada por apenas um caractere.

## Veja Também

- [Documentação oficial do pacote unicode/utf8](https://pkg.go.dev/unicode/utf8)
- [Exemplos práticos de uso da função len()](https://www.geeksforgeeks.org/golang-sting-len-function/)
- [Vídeo tutorial sobre o uso da função len() em Go](https://www.youtube.com/watch?v=nL5sponzVQE)