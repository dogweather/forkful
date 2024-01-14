---
title:                "Haskell: Comparando duas datas"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que Comparar Duas Datas Pode Ser Útil

Comparar duas datas é uma tarefa comum em muitos programas, especialmente aqueles que lidam com informações relacionadas ao tempo. Ao comparar duas datas, podemos determinar qual é mais recente, qual ocorreu primeiro ou se ambas são iguais. Isso pode ser útil em diversas situações, como no gerenciamento de eventos, no cálculo de duração de um período e na organização de dados temporais.

## Como Comparar Duas Datas em Haskell

Para comparar duas datas em Haskell, podemos utilizar a função `compare` do módulo `Data.Time`. Essa função recebe dois valores do tipo `Day` e retorna um tipo `Ordering` que indica se a primeira data é menor, igual ou maior que a segunda data. Veja um exemplo de código abaixo:

```Haskell
import Data.Time

main = do
  let data1 = fromGregorian 2021 07 15
  let data2 = fromGregorian 2021 04 12
  print $ compare data1 data2
```

A saída desse exemplo seria `GT`, indicando que a primeira data é maior que a segunda. É importante notar que a função `compare` só funciona com datas no formato `Day` do módulo `Data.Time`, portanto, é necessário converter as datas para esse formato antes de realizar a comparação.

## Mergulho Profundo na Comparação de Datas

Para entender melhor como a função `compare` funciona, podemos nos aprofundar um pouco mais no conceito de ordenação em Haskell. A função `compare` utiliza o conceito de *ordem parcial* para realizar comparações entre valores. Isso significa que nem sempre é possível determinar de forma definitiva a ordem entre dois valores, pois eles podem ser igualmente importantes ou não possuem uma relação de precedência.

No caso das datas, as comparações geralmente são feitas comparando primeiro os anos, depois os meses e finalmente os dias. Vale ressaltar que a função `compare` também pode ser utilizada para comparar outros tipos de dados, desde que eles sejam instâncias da classe `Ord`.

## Veja Também

- [Documentação do módulo `Data.Time`](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Tutorial de Haskell do Livro Learn You a Haskell for Great Good!](http://learnyouahaskell.com/starting-out#comparison)
- [Artigo sobre ordem parcial em Haskell](https://wiki.haskell.org/Ordering)