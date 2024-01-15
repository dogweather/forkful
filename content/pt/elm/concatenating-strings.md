---
title:                "Concatenação de strings"
html_title:           "Elm: Concatenação de strings"
simple_title:         "Concatenação de strings"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que

Você já se perguntou por que precisamos concatenar strings ao programar em Elm? A resposta é simples: para unir duas ou mais strings em uma única string. Isso pode ser útil em diversas situações, como na construção de mensagens de erro ou na criação de caminhos de arquivo.

## Como fazer

Para concatenar strings em Elm, podemos usar o operador `++`. Veja dois exemplos de como utilizá-lo:

```Elm
nomeCompleto = "João" ++ " Silva"

mensagem = "Bem-vindo, " ++ nomeCompleto
```

Nesse primeiro exemplo, estamos criando uma variável `nomeCompleto` que recebe a concatenação da string "João" com a string "Silva". Em seguida, criamos outra variável `mensagem` que utiliza a variável `nomeCompleto` para criar uma saudação personalizada.

Outra forma de concatenar strings em Elm é utilizando a função `String.concat`. Ela permite unir várias strings em uma única, como no exemplo a seguir:

```Elm
nomes = ["Maria", "Ana", "Laura"]

nomesComVirgula = String.concat ", " nomes
```

Nesse caso, a saída será a string "Maria, Ana, Laura", pois usamos a função `String.concat` para unir os elementos da lista `nomes` separados por vírgula.

## Aprofundando mais

Ao concatenar strings em Elm, é importante lembrar que as strings originais não são alteradas, pois a concatenação cria uma nova string. Além disso, é possível usar a concatenação em conjunto com outras funções, como `String.toUpper` para transformar todas as letras em maiúsculas ou `String.startsWith` para verificar se uma string começa com determinados caracteres.

Uma dica importante é evitar usar a concatenação em loop, pois isso pode causar problemas de performance. Em vez disso, prefira utilizar a função `String.concat` quando precisar unir várias strings em uma lista.

## Veja também

- [Documentação oficial do Elm](https://elm-lang.org/docs)
- [Tutorial de Elm para iniciantes](https://www.tutorialspoint.com/elm/index.htm)
- [Exemplos de código em Elm](https://github.com/elm/example)