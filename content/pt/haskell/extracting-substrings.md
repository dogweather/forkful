---
title:                "Extraindo subtrings"
html_title:           "Haskell: Extraindo subtrings"
simple_title:         "Extraindo subtrings"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que

Se você é um programador e está familiarizado com Haskell, provavelmente já ouviu falar sobre a função `substring` e como ela pode ser útil para extrair partes específicas de uma string. Mas por que alguém se interessaria em fazer isso? Bem, há várias razões pelas quais você pode querer extrair substrings, como manipular dados, formatar saídas de texto ou até mesmo realizar análises de dados.

## Como Fazer

Extrair substrings em Haskell é bastante simples. Primeiro, precisamos importar o módulo `Data.List` para ter acesso à função `substring`. Então, podemos usar a seguinte sintaxe:

```Haskell
substring startIndex length string
```
O parâmetro `startIndex` indica onde a substring deve começar, `length` indica o comprimento da substring e `string` é a string original da qual queremos extrair a substring. Vamos ver um exemplo prático:

```Haskell
import Data.List

substring 3 5 "Olá mundo" -- retorna "a mun"
```

Também podemos usar números negativos para `startIndex` e `length` para extrair substrings a partir do fim da string. Por exemplo:

```Haskell
substring (-3) 2 "Olá mundo" -- retorna "nd"
```

E se quisermos extrair todo o resto da string a partir de um determinado ponto, podemos usar `undefined` para `length`, indicando que não há um limite de comprimento. Veja:

```Haskell
substring 3 undefined "Olá mundo" -- retorna "a mundo"
```

## Profundidade

Agora que você entende como usar a função `substring` em Haskell, vamos dar uma olhada mais profunda em como ela funciona. Internamente, essa função podemos ver um cabeçalho como este:

```Haskell
substring :: Int -> Int -> [a] -> [a]
```

Onde `a` é um tipo de dados qualquer. Isso significa que podemos usar `substring` não apenas com strings, mas com qualquer lista de tipos de dados. Além disso, a definição real da função usa a função `drop` do módulo `Data.List` para remover os elementos antes do `startIndex` e a função `take` para obter o número de elementos especificado por `length`.

Outro detalhe interessante é que, se o `startIndex` informado for maior do que o tamanho da string, a função retornará uma lista vazia. E se o `length` for maior do que o número de elementos restantes após `startIndex`, a função retornará uma substring com todos esses elementos restantes.

## Veja Também

Aqui estão alguns links úteis para saber mais sobre a função `substring` em Haskell:

- [Documentação oficial do `Data.List`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:take)
- [Tutorial sobre strings em Haskell](https://learnxinyminutes.com/docs/pt-br/haskell-pt/)
- [Outras funções úteis para trabalhar com strings em Haskell](https://wiki.haskell.org/String)