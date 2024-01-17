---
title:                "Encontrando o comprimento de uma string."
html_title:           "Haskell: Encontrando o comprimento de uma string."
simple_title:         "Encontrando o comprimento de uma string."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O que & Por Que?

Encontrar o comprimento de uma string é uma tarefa comum em programação, pois permite aos desenvolvedores saber quantos caracteres uma determinada string possui. Isso é útil para várias aplicações, como validação de entrada do usuário, manipulação de texto e formatação de saída.

## Como Fazer:

Para encontrar o comprimento de uma string em Haskell, podemos usar a função `length` da biblioteca padrão `Prelude`. Veja abaixo um exemplo de código:

```Haskell
-- Definir uma string
let minhaString = "Hello World!"
-- Usar a função `length` para encontrar o comprimento
length minhaString
-- Saída: 12
```

Observe que a função `length` retorna um valor do tipo `Int`, que representa um número inteiro.

## Mergulho Profundo:

A função `length` em Haskell é baseada no conceito de lista. Uma lista é uma estrutura de dados que pode armazenar múltiplos valores de um mesmo tipo. No caso de uma string, cada caractere é considerado um elemento da lista.

Existem também outras formas de encontrar o comprimento de uma string em Haskell, como usando a função `genericLength` da biblioteca `Data.List`, que retorna um valor do tipo `Integer`.

## Veja Também:

- [Documentação da função `length` em Haskell](https://hackage.haskell.org/package/base/docs/Prelude.html#v:length)
- [Tutorial sobre strings em Haskell](https://www.tutorialspoint.com/haskell/haskell_strings.htm)
- [Documentação da função `genericLength` em Haskell](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html#v:genericLength)