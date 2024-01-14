---
title:                "Haskell: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que Comparar Duas Datas

Ao escrever programas em Haskell, muitas vezes surge a necessidade de comparar duas datas. Pode ser para verificar qual data é a mais recente ou para determinar a diferença entre duas datas. Independentemente do motivo, é importante entender como comparar datas em Haskell para escrever um código eficiente e livre de erros.

## Como Fazer

Comparar duas datas em Haskell é bastante simples. Primeiro, é necessário importar o módulo `Data.Time` que contém as funções necessárias para lidar com datas. Em seguida, podemos criar duas variáveis contendo as datas a serem comparadas, utilizando o formato `UTCTime`. Por exemplo:

```
import Data.Time

dia_1 = UTCTime (fromGregorian 2020 10 10) (secondsToDiffTime 0)
dia_2 = UTCTime (fromGregorian 2020 10 11) (secondsToDiffTime 0)
```

Com as duas datas criadas, podemos utilizar as funções `compare` e `diffUTCTime` para compará-las e determinar a diferença entre elas, respectivamente. Por exemplo:

```
compare dia_1 dia_2  -- Resultado: LT (menor que)
diffUTCTime dia_1 dia_2  -- Resultado: 86400.0 (1 dia em segundos)
```

É importante notar que a função `diffUTCTime` retorna a diferença em segundos, por isso é necessário fazer a conversão para o formato desejado (dias, horas, minutos, etc).

## Mergulho Profundo

Em Haskell, datas são representadas pelo tipo `UTCTime`, que contém o dia e o horário em segundos. Portanto, a função `compare` compara primeiramente o dia e, em caso de empate, compara os horários. Já a função `diffUTCTime` simplesmente subtrai o horário da primeira data pelo horário da segunda data para obter a diferença em segundos. Além disso, é possível utilizar outros formatos de data, como `ZonedTime` e `LocalTime`, dependendo da necessidade do programa. Para mais detalhes sobre a biblioteca `Data.Time`, consulte a documentação oficial [aqui](https://hackage.haskell.org/package/time/docs/Data-Time.html).

## Veja Também

- [Documentação oficial do módulo `Data.Time`](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Tutorial sobre como lidar com datas em Haskell](https://www.haskellforall.com/2019/04/haskell-dates.html)
- [Comparando datas em outras linguagens de programação](https://www.guru99.com/date-time-and-epoch-time.html)