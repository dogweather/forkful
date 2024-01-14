---
title:    "Go: Encontrando o comprimento de uma string"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Por que

Você já se perguntou como os programas de computador conseguem saber quantos caracteres existem em uma frase ou palavra? Isso é possível por meio da função "len" em linguagens de programação, incluindo Go. Descubra como encontrar o comprimento de uma string usando Go neste artigo.

## Como fazer

Para encontrar o comprimento de uma string em Go, você precisará usar a função "len". Essa função retorna o número de bytes na string e pode ser usada em qualquer tipo de string, incluindo palavras, frases e até mesmo números.

Veja um exemplo de código abaixo:

```
package main

import "fmt"

func main() {
    str := "Olá, mundo!"
    fmt.Printf("O comprimento da string é: %d", len(str))
}
```

Ao compilar e executar este código, você verá o seguinte resultado:

```
O comprimento da string é: 12
```

Como você pode ver, a função "len" retornou o número de caracteres na string "Olá, mundo!".

## Mais detalhes

A função "len" em Go é bastante simples, mas é importante entender alguns detalhes extras. Em primeiro lugar, a função "len" pode ser usada em qualquer tipo de dado que tenha um tamanho definido, incluindo arrays e slices. Além disso, a função não calcula o tamanho das strings em termos de palavras, mas sim de bytes. Isso significa que caracteres acentuados ou especiais contam como mais de um byte, e isso pode afetar o resultado da função.

Outra coisa importante a se notar é que a função "len" não funciona em tipos de dados que não possuem um tamanho definido, como mapas ou estruturas. Nestes casos, você precisará encontrar o tamanho dos dados manualmente.

## Veja também

Aqui estão alguns links para você explorar mais sobre a função "len" em Go:

- [A documentação oficial da função "len" em Go](https://golang.org/ref/spec#Length_and_capacity)
- [Uma discussão sobre o uso correto da função "len"](https://medium.com/@meeusdylan/how-to-use-the-length-function-in-golang-9164b8d0e4b2)
- [Um exemplo prático da função "len" em um programa](https://golangbyexample.com/len-function-golang/)

Agora que você sabe como encontrar o comprimento de uma string em Go, experimente usar a função "len" em seus próximos projetos e veja como ela pode ser útil.