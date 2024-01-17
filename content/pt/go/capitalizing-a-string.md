---
title:                "Capitalizando uma string"
html_title:           "Go: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O que e por que?

Capitalizar uma string em Go significa transformar a primeira letra de cada palavra em maiuscula. Programadores geralmente fazem isso para padronizar a formatacao de textos e torna-los mais legiveis.

## Como fazer:

Para capitalizar uma string em Go, podemos usar a funcao ```strings.Title()```. Veja o exemplo abaixo para entender como funciona:

```
package main

import (
    "fmt"
    "strings"
)

func main() {
    texto := "ola, mundo!"
    fmt.Println(strings.Title(texto))
}

```

Output:
```
Ola, Mundo!
```

## Profundando:

Historicamente, a capitalizacao de strings era feita manualmente, com a conversao de letras para maiusculas usando as funcoes ```strings.ToUpper()``` ou ```strings.Title()```. No entanto, com a introducao do pacote ```unicode```, agora e possivel capitalizar strings de forma mais precisa, levando em consideracao caracteres especiais e acentuacao.

## Veja tambem:

[Documentacao da funcao strings.Title() em Go](https://golang.org/pkg/strings/#Title)

[Exemplos de uso do pacote unicode em Go](https://www.geeksforgeeks.org/unicode-package-in-go-lang-with-examples/)