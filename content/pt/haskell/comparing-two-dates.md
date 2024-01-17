---
title:                "Comparando duas datas"
html_title:           "Haskell: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Comparar duas datas é o processo de verificar se uma data é maior, menor ou igual a outra. Isso pode ser útil em programação quando precisamos classificar eventos por ordem cronológica ou realizar cálculos de tempo entre duas datas.

## Como Fazer:

```
compararDatas :: Data -> Data -> Ordering
compararDatas data1 data2
    | data1 > data2 = GT
    | data1 < data2 = LT
    | otherwise = EQ
```

Para comparar duas datas em Haskell, podemos usar a função pré-definida `Ord`. Essa função recebe duas datas e retorna um tipo de dados `Ordering` que representa se a primeira data é maior (`GT`), menor (`LT`) ou igual (`EQ`) à segunda data.

Exemplo de input:
```
compararDatas (Data 10 03 2020) (Data 05 03 2020)
```
Output esperado: `GT`

## Deep Dive:
Comparar datas é uma tarefa comum em programação, especialmente quando lidamos com agendamentos, tarefas ou cálculos envolvendo tempo. Em Haskell, podemos utilizar a biblioteca `Data.Time` para trabalhar com datas de forma mais complexa, incluindo cálculos entre datas, manipulação de fusos horários, entre outros recursos.

Uma alternativa para comparar datas em Haskell é utilizando a biblioteca `Chiphunk`, que oferece funções específicas para lidar com datas no formato ISO 8601.

A implementação da função `Ord` em Haskell utiliza o conceito de "typeclass", que permite que diferentes tipos de dados possam ser ordenados de forma flexível. Isso significa que podemos aplicar a função `Ord` em diferentes tipos de dados, desde que esses tipos tenham uma instância da classe `Ord` implementada.

## Veja também:
- [Haskell: Aprenda a programar](https://www.casadocodigo.com.br/products/livro-aprenda-haskell) - Livro de introdução à linguagem Haskell.
- [Documentação oficial do Haskell](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/) - Referência completa sobre a linguagem.
- [Chiphunk](https://hackage.haskell.org/package/chiphunk) - Biblioteca para manipulação de datas em Haskell no formato ISO 8601.