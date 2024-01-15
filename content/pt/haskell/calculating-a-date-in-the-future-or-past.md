---
title:                "Calculando uma data no futuro ou passado"
html_title:           "Haskell: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Calcular uma data no futuro ou no passado pode ser uma tarefa útil em diversas situações, como planejar eventos ou automatizar tarefas. Além disso, a linguagem Haskell oferece uma sintaxe simples e poderosa para lidar com cálculos de datas.

## Como fazer

Para calcular uma data em Haskell, utilizaremos a função `addDays` da biblioteca `Data.Time.Calendar`. Essa função recebe dois parâmetros: o número de dias que desejamos adicionar ou subtrair da data atual, e a data de referência.

```Haskell
import Data.Time.Calendar

dataAtual = fromGregorian 2021 02 08 -- 08 de fevereiro de 2021
dataFutura = addDays 30 dataAtual -- adiciona 30 dias à data atual
dataPassado = addDays (-15) dataAtual -- subtrai 15 dias da data atual

-- Resultado:
-- dataFutura = 2021-03-10
-- dataPassado = 2021-01-24
```

O exemplo acima utiliza o tipo `Day` para representar datas, mas também é possível utilizar tipos mais específicos, como `LocalDate` e `ZonedTime` da biblioteca `Data.Time.LocalTime`.

## Mergulho profundo

Além de adicionar ou subtrair dias, a biblioteca `Data.Time.Calendar` oferece outras funções úteis para cálculos de datas, como `addMonths`, `addYears` e `diffDays`. Também é possível trabalhar com horários e fusos horários utilizando as bibliotecas `Data.Time.LocalTime` e `Data.Time.Zones`.

Você também pode criar suas próprias funções de cálculos de datas, utilizando a poderosa sintaxe de pattern matching de Haskell. Por exemplo, podemos criar uma função que soma uma semana à data atual:

```Haskell
import Data.Time.Calendar

addWeek :: Day -> Day
addWeek (fromGregorian ano mes dia) = fromGregorian ano mes (dia + 7)

-- Exemplo:
addWeek (fromGregorian 2021 02 08) -- resultado: 2021-02-15
```

Com isso, é possível realizar cálculos de datas de forma simples e precisa em seus projetos em Haskell.

## Veja também

- Documentação da biblioteca `Data.Time.Calendar`: http://hackage.haskell.org/package/time/docs/Data-Time-Calendar.html
- Documentação da biblioteca `Data.Time.LocalTime`: http://hackage.haskell.org/package/time/docs/Data-Time-LocalTime.html
- Documentação da biblioteca `Data.Time.Zones`: https://hackage.haskell.org/package/timezone-series-0.1.11/docs/Data-Time-Zones.html