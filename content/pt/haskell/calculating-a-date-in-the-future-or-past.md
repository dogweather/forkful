---
title:                "Haskell: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que?

Calcular datas no futuro ou passado pode ser uma tarefa útil em diversas situações, como por exemplo para programar eventos ou criar um sistema de agendamento. Além disso, pode ser uma oportunidade para aprender novas técnicas e aprimorar suas habilidades em Haskell.

## Como Fazer

Para realizar esse cálculo em Haskell, podemos utilizar funções da biblioteca "Data.Time". Primeiramente, importaremos essa biblioteca no início do nosso código:

```Haskell
import Data.Time
```

Em seguida, podemos criar uma função que recebe uma data inicial e uma quantidade de dias a serem adicionados, e retorna a nova data calculada:

```Haskell
addDays :: Day -> Integer -> Day
addDays initialDate days = addDays days initialDate
```

Podemos testar essa função passando uma data inicial e um número de dias desejado:

```Haskell
-- Calculando a data 10 dias a partir de hoje
addDays (fromGregorian 2021 04 01) 10
-- Output: 2021-04-11
```

Além disso, podemos utilizar outras funções da biblioteca como "addMonths" e "addYears" para calcular datas em diferentes períodos. É importante lembrar de checar a documentação da biblioteca para entender melhor como as funções são utilizadas e quais argumentos elas recebem.

## Mergulho Profundo

Além das funções mencionadas acima, a biblioteca "Data.Time" conta com diversas outras que podem ser úteis para calcular datas em Haskell. Por exemplo, a função "diffDays" permite calcular a diferença entre duas datas em dias, enquanto a função "isLeapYear" verifica se um ano é bissexto ou não. É possível combinar essas funções e criar algoritmos mais complexos para lidar com datas.

Além disso, podemos utilizar outras bibliotecas como "Data.Dates" e "Data.Time.Format" para realizar formatações de datas e manipulações mais avançadas. Com um pouco de pesquisa e prática, é possível se tornar um expert em lidar com datas em Haskell.

## Veja Também

- Referência da biblioteca "Data.Time": https://hackage.haskell.org/package/time 
- Documentação da biblioteca "Data.Dates": https://hackage.haskell.org/package/dates
- Como lidar com datas em Haskell: https://wiki.haskell.org/Handling_time_zone_info