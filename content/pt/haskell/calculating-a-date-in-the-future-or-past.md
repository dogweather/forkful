---
title:    "Haskell: Calculando uma data no futuro ou no passado."
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Você já se perguntou como os aplicativos de calendário ou de planejamento de viagens calculam datas no futuro ou no passado? Aprender a calcular datas em Haskell pode ser útil em vários projetos de programação e pode até mesmo ser usado para criar seu próprio aplicativo de calendário!

## Como Fazer

Em Haskell, podemos usar a biblioteca "Time" para calcular datas no futuro ou no passado. Primeiro, importamos a biblioteca com o comando ```Haskell import Data.Time```. Em seguida, podemos usar a função ```Haskell addDays ``` para adicionar ou subtrair dias de uma determinada data. Veja o exemplo abaixo:

```Haskell 
import Data.Time

futureDate = addDays 100 (fromGregorian 2020 12 31)
-- resultado: 2021-04-10

pastDate = addDays (-30) (fromGregorian 2021 02 15)
-- resultado: 2021-01-16
```

Neste exemplo, estamos adicionando 100 dias à data 31 de dezembro de 2020 e subtraindo 30 dias da data 15 de fevereiro de 2021. A função ```fromGregorian ``` é usada para criar uma data no formato (ano, mês, dia).

## Mergulho Profundo

Além de adicionar ou subtrair dias, a biblioteca "Time" também possui funções para calcular datas com base em outras unidades de tempo, como semanas, meses e anos. Também é possível trabalhar com horas e minutos usando a função ```addUTCTime ```.

Além disso, a biblioteca "Time" possui outras funções úteis, como converter datas para outros formatos ou comparar datas. Explorar todas as funcionalidades dessa biblioteca pode ser muito interessante para desenvolver projetos mais complexos em Haskell.

## Veja também

- Documentação da biblioteca "Time": https://hackage.haskell.org/package/time
- Tutorial sobre cálculo de datas em Haskell: https://wiki.haskell.org/Date_-_although_time_central
- Exemplos de uso da biblioteca "Time": https://github.com/bos/attoparsec/blob/master/test/Benchmarks/Csv.hs