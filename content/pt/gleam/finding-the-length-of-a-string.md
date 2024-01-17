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

## O que e por que?

Encontrar o comprimento de uma string é o ato de determinar quantos caracteres estão presentes em uma determinada string. Isso é útil para os programadores porque muitas vezes precisamos saber o tamanho de uma string para fins de validação, manipulação ou exibição.

## Como fazer:

Para encontrar o comprimento de uma string em Gleam, usamos a função `size` seguida do nome da string entre parênteses. Veja o exemplo abaixo:

```
Gleam

let string = "Olá, mundo!"
let length = size(string)

```

Assim, a variável `length` será igual a 12, pois a string possui 12 caracteres (incluindo o espaço em branco). Você também pode encontrar o comprimento de uma string diretamente, sem precisar armazenar em uma variável, como no exemplo abaixo:

```
Gleam

assert size("Oi!") == 3

```

Este exemplo usa a função `assert` para verificar se o comprimento da string "Oi!" é igual a 3. Se sim, o código continua executando normalmente. Caso contrário, um erro é retornado.

## Profundidade:

Encontrar o comprimento de uma string é uma tarefa bastante comum em programação. Na verdade, é uma das funções básicas disponíveis em muitas linguagens de programação. Além disso, existem outras formas de encontrar o comprimento de uma string, como usar um loop para contar cada caracter ou usar a função `len` em outras linguagens.

Em termos de implementação, a função `size` em Gleam é definida da seguinte forma:

```
Gleam

fn size(string: String) -> Int {
  Bytes.size(string.bytes)
}

```

Isso significa que a função `size` vai primeiro converter a string em uma sequência de bytes e então determinar o tamanho dessa sequência. Isso pode ser útil se a string não for composta apenas de caracteres ASCII.

## Veja também:

- [A documentação oficial do Gleam sobre strings](https://gleam.run/book/callables.html#strings)
- [Um artigo sobre a função `len` em Python](https://www.freecodecamp.org/news/python-len-function-tutorial/)
- [Uma discussão sobre a diferença entre `size` e `len` em programação em geral](https://stackoverflow.com/questions/2822688/difference-between-len-and-size-in-python)