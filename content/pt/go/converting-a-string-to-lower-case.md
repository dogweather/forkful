---
title:                "Convertendo uma string para minúsculas"
html_title:           "Fish Shell: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Transformando String para Minúsculas em Go

## O Que & Por Que?

Transformar uma string em minúsculas é o processo de converter todas as letras maiúsculas de um texto para minúsculas. Os programadores fazem isso frequentemente para normalizar as entradas de texto, facilitando a comparação e a busca.

## Como Faz:

Em Go, utilizamos a função `ToLower` do pacote `strings` para converter todas as letras maiúsculas em minúsculas. Olha só:

```Go
package main

import (
    "fmt"
    "strings"
)

func main () {
    texto := "Olá, Mundo!"
    minusc := strings.ToLower(texto)
    fmt.Println(minusc)
}
```

A saída seria:

```Go
olá, mundo!
```

## Mais Detalhes:

Historicamente, a normalização de texto foi crucial para sistemas de busca e banco de dados, pois permite que a busca seja insensível à caixa. Em Go, a função `ToLower` transforma a string utilizando o mapeamento Unicode para minúsculas.

Outras alternativas em Go incluem a função `ToLowerSpecial` que aceita um parâmetro 'c' específico que determina o tipo de mapeamento de caso. Por exemplo, `ToLowerSpecial(unicode.TurkishCase, texto)`, que irá fazer a conversão levando em conta as regras específicas do idioma turco.

## Veja Também:

Documentação oficial da biblioteca de strings do Go : https://golang.org/pkg/strings/
Exemplos e explicações mais detalhadas : https://gobyexample.com/string-functions