---
title:                "Haskell: Extração de subcadeias"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que

Extração de substrings é uma habilidade importante em programação Haskell, já que permite que os desenvolvedores manipulem strings de forma eficiente e precisa. Além disso, essa técnica é útil em diversas aplicações práticas, como manipulação de dados e processamento de linguagem natural.

## Como fazer

Para extrair uma substring em Haskell, podemos usar a função `take` ou `drop`, dependendo do que queremos obter. Por exemplo, para extrair os primeiros 5 caracteres de uma string, usamos `take 5 "Hello World"`, e o resultado será "Hello". Também podemos usar índices negativos, como em `drop (-3) "Hello World"`, que irá remover os últimos 3 caracteres e retornar "Hello".

```Haskell
-- Exemplo de uso da função take
take 3 "abcde" -- Resultado: "abc"

-- Exemplo de uso da função drop
drop 2 "abcde" -- Resultado: "cde"
```

Outra forma de extrair substrings em Haskell é usando a função `substring` da biblioteca `Data.List`. Esta função recebe três parâmetros: o índice inicial, o comprimento da substring e a string original. Por exemplo, `substring 2 4 "Hello World"` irá retornar "llo ".

```Haskell
import Data.List (substring)

-- Exemplo de uso da função substring
substring 1 5 "Hello World" -- Resultado: "ello "
```

## Aprofundando

Para entender melhor como a extração de substrings funciona em Haskell, é importante conhecer alguns conceitos-chave. Em Haskell, as strings são tratadas como listas de caracteres, o que nos permite usar as funções `take` e `drop` diretamente nelas. Além disso, as strings também podem ser representadas como matrizes de caracteres, o que possibilita o uso da função `substring`.

Outro ponto importante é o uso de índices na extração de substrings. Em Haskell, os índices começam com o número 0, o que significa que o primeiro elemento da lista tem o índice 0, o segundo tem o índice 1, e assim por diante. Além disso, como mencionado anteriormente, também podemos usar índices negativos para contar do final da lista.

## Veja também

- [Documentação da função `take` em Haskell](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Tuple.html#v:take)
- [Documentação da função `drop` em Haskell](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Tuple.html#v:drop)
- [Documentação da função `substring` em Haskell](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:substring)