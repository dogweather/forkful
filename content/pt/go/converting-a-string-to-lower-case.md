---
title:                "Go: Convertendo uma string para letras minúsculas"
simple_title:         "Convertendo uma string para letras minúsculas"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que converter uma string para minúsculas em Go?

Converter uma string para minúsculas é uma tarefa comum em muitos programas. Pode ser útil para facilitar a comparação de strings, tornar a saída de texto mais consistente ou até mesmo lidar com entradas de usuários que podem estar em maiúsculas ou minúsculas. Em Go, essa tarefa pode ser realizada de forma rápida e fácil com algumas linhas de código. Vamos descobrir como!

## Como fazer: Convertendo uma string para minúsculas em Go

Para converter uma string para minúsculas em Go, podemos usar a função `ToLower` do pacote `strings`. Veja um exemplo abaixo:

```Go
package main

import (
  "fmt"
  "strings"
)

func main() {
  str := "Olá, MUNDO!"
  lower := strings.ToLower(str)
  fmt.Println(lower)
}
```

A saída deste código será `olá, mundo!`, com a string original convertida para minúsculas. Em resumo, basta usar a função `ToLower` passando a string que deseja converter como parâmetro, e a própria função irá retornar a string convertida.

## Dica avançada: Usando o pacote unicode para conversões multilíngue

Embora a função `ToLower` seja suficiente para a maioria dos casos, ela pode não ser adequada para idiomas que possuem caracteres especiais, como o português. Nesses casos, é recomendável usar o pacote `unicode` para garantir que a conversão seja feita corretamente. Veja um exemplo abaixo:

```Go
package main

import (
  "fmt"
  "strings"
  "unicode"
)

func main() {
  str := "Ão, CoISa"
  lower := strings.Map(unicode.ToLower, str)
  fmt.Println(lower)
}
```

A saída neste caso será `ão, coisa`, com o caracter especial "Ã" sendo convertido corretamente para "ã". Isso ocorre porque a função `Map` do pacote `strings` permite a passagem de uma função para ser aplicada a cada caractere da string, e podemos usar a função `ToLower` do pacote `unicode` para tratar caracteres especiais.

## Veja também

- [Documentação oficial sobre a função ToLower](https://pkg.go.dev/strings#ToLower)
- [Manuseio de strings em Go](https://www.calhoun.io/manipulating-strings-in-go/)

Agora que você aprendeu como converter uma string para minúsculas em Go, experimente aplicar esse conhecimento em seus projetos. E lembre-se, para casos mais avançados, o pacote `unicode` é seu aliado!