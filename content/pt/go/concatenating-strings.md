---
title:    "Go: Concatenando strings"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que concatenar strings em Go é importante

Ao escrever um programa em Go, muitas vezes podemos precisar combinar várias strings em uma única string. Isso é chamado de concatenação de strings e pode ser muito útil em várias situações, como formatar saídas de texto ou construir URLs dinâmicas.

## Como implementar a concatenação de strings em Go 

Para concatenar strings em Go, podemos usar o operador de adição `+` ou a função `fmt.Sprintf()`. Vamos dar uma olhada em alguns exemplos para entender melhor:

```
package main

import "fmt"

func main() {
    // Usando o operador de adição
    nome := "João"
    sobrenome := "Silva"
    nomeCompleto := nome + " " + sobrenome
    fmt.Println(nomeCompleto)

    // Usando a função fmt.Sprintf()
    idade := 25
    texto := fmt.Sprintf("João tem %d anos", idade)
    fmt.Println(texto)
}
```
```
Saída:
João Silva
João tem 25 anos
```

No primeiro exemplo, usamos o operador de adição para combinar as duas strings `nome` e `sobrenome` com um espaço entre elas. No segundo exemplo, usamos a função `fmt.Sprintf()` para formatar uma string com base em uma variável `idade` que foi inserida no meio do texto.

## Mergulhando mais fundo na concatenação de strings em Go 

Em Go, os valores strings são imutáveis, ou seja, não podem ser alterados após sua criação. Portanto, sempre que concatenamos strings, é criada uma nova string contendo o resultado da concatenação. Isso pode não ser eficiente quando temos muitas strings para combinar.

Uma alternativa mais eficiente é usar o pacote `strings` e sua função `Join()`, que funciona como uma concatenação de strings em massa. Vamos ver um exemplo:

```
package main

import (
    "fmt"
    "strings"
)

func main() {
    palavras := []string{"Olá", "meu", "nome", "é", "Maria"}
    nomeCompleto := strings.Join(palavras, " ")
    fmt.Println(nomeCompleto)
}
```
```
Saída:
Olá meu nome é Maria
```

Neste exemplo, definimos um slice de strings com as palavras que queremos combinar. Em seguida, usamos a função `Join()` do pacote `strings` para unir essas palavras em uma única string, adicionando um espaço entre elas.

## Veja também

- [Documentação oficial do pacote strings](https://golang.org/pkg/strings/)
- [Tutorial sobre concatenação de strings em Go](https://www.digitalocean.com/community/tutorials/how-to-format-strings-in-go-pt)
- [Vídeo sobre concatenação de strings em Go](https://www.youtube.com/watch?v=JLnj6wNy4_o)