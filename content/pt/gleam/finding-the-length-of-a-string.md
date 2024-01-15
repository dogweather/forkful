---
title:                "Encontrando o comprimento de uma string"
html_title:           "Gleam: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que 

Existem muitas situações em que precisamos saber o tamanho de uma string em nosso código. Isso pode ser útil para verificação de entrada do usuário, manipulação de dados de texto ou até mesmo para fins de formatação.

## Como fazer

Para encontrar o tamanho de uma string em Gleam, podemos usar a função `String.length/1` que retorna o número de caracteres em uma string. Vamos ver um exemplo abaixo:

```Gleam
let string = "Olá!"
let tamanho = String.length(string)
io.println(tamanho)
```

Isso irá imprimir `4` na tela, já que existem quatro caracteres na string "Olá!".

Também podemos encontrar o tamanho de uma string de maneira mais dinâmica, permitindo que o usuário insira sua própria string e receba o tamanho como resultado:

```Gleam
io.print("Insira uma string: ")
let string = io.readLine()
let tamanho = String.length(string)
io.println("O tamanho da string é:", tamanho)
```

## Profundando

Ao trabalhar com strings, é importante lembrar que o tamanho retornado pela função `String.length/1` significa o número de caracteres, e não o número de palavras ou espaços. Por exemplo:

```Gleam
let string = "Olá, mundo!"
let tamanho = String.length(string)
io.println(tamanho) // retorna 12, contanto com o espaço e a vírgula
```

Outra coisa a se ter em mente é que a função `String.length/1` também considera caracteres especiais ou acentuados, portanto, se sua string contém letras com acentos, o tamanho será maior do que o número de caracteres visíveis.

Além disso, é possível manipular o tamanho de uma string usando outras funções, como `String.slice/3` ou `String.concat/2`. Experimente brincar com essas funções para ver o que você pode fazer!

## Veja também

- [Funções de strings em Gleam](https://gleam.run/modules/stdlib/String.html)
- [Documentação Gleam](https://gleam.run/documentation/)