---
title:    "Go: Concatenando Strings"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Por que Concatenar Strings é Importante em Go

Concatenar strings é uma tarefa essencial na programação Go. É útil quando queremos combinar diferentes strings em uma única string. Isso é especialmente útil ao trabalhar com entradas do usuário ou ao criar saídas personalizadas.

## Como Concatenar Strings em Go

A concatenação de strings em Go pode ser feita de forma simples usando o operador `+`. Veja um exemplo abaixo:

```Go
package main

import "fmt"

func main() {
  nome := "Maria"
  sobrenome := "Silva"

  nomeCompleto := nome + " " + sobrenome
  fmt.Println(nomeCompleto)
}
```

Saída:
```
Maria Silva
```

Neste exemplo, usamos o operador `+` para combinar as strings `nome` e `sobrenome`, e armazenamos o resultado na variável `nomeCompleto`.

## Profundidade em Concatenar Strings

Uma coisa importante a se notar é que o operador `+` em Go só pode ser usado para concatenar duas strings de cada vez. Se você precisar concatenar mais de duas strings, pode usar a função `Join` do pacote `strings`. Veja um exemplo abaixo:

```Go
package main

import (
  "fmt"
  "strings"
)

func main() {
  palavras := []string{"Olá", "mundo", "!"}

  resultado := strings.Join(palavras, " ")
  fmt.Println(resultado)
}
```

Saída:
```
Olá mundo!
```

Além disso, é importante ter em mente que a concatenação de strings em Go é imutável, o que significa que, ao concatenar strings, uma nova string é criada em vez de ser modificada. Portanto, é importante atribuir o resultado da concatenação a uma variável para que possa ser usado posteriormente.

## Veja também

- [Documentação oficial sobre strings em Go](https://golang.org/pkg/strings/)
- [Tutorial de Go: Concatenando strings](https://www.tutorialspoint.com/go/go_string_concatenation.htm)
- [Vídeo tutorial: Entendendo como funciona a concatenação de strings em Go](https://www.youtube.com/watch?v=bXqlUdmaKrE)