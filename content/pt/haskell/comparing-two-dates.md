---
title:    "Haskell: Comparando duas datas"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Por que?

Comparar datas é uma tarefa comum em programação, seja para verificar a diferença entre duas datas, ordenar eventos em uma linha do tempo ou validar o formato de uma data. Portanto, é importante conhecer como realizar essa operação de forma eficiente e precisa em Haskell.

## Como fazer

Para comparar duas datas em Haskell, podemos utilizar a função `compare` do módulo `Data.Ord`. Essa função recebe dois valores e retorna um tipo `Ordering` que indica se o primeiro valor é menor, igual ou maior que o segundo valor. Por exemplo, se quisermos comparar duas datas do tipo `Day`, podemos utilizar o seguinte código:

```Haskell
import Data.Ord (compare)
import Data.Time.Calendar (Day, fromGregorian)

dia1 :: Day
dia1 = fromGregorian 2021 07 20

dia2 :: Day
dia2 = fromGregorian 2021 07 25

resultado :: Ordering
resultado = compare dia1 dia2

-- Output: LT
```

No exemplo acima, utilizamos a função `fromGregorian` para criar duas datas e a função `compare` para compará-las. O resultado da comparação foi `LT`, o que indica que o primeiro dia é anterior ao segundo.

Podemos ainda utilizar outros tipos de dados, como `UTCTime` ou `LocalTime`, e compará-los de maneira similar. Além disso, é possível utilizar as funções `min` e `max` para encontrar a menor e a maior data entre duas datas.

## Mergulho profundo

Ao comparar datas em Haskell, é importante ter em mente que a comparação é feita de forma lexicográfica. Isso significa que as datas são comparadas por ordem de ano, mês e dia. Por exemplo, se tivermos as datas `2021-07-20` e `2020-08-05`, a primeira é considerada maior que a segunda pois o ano é mais recente.

Outro detalhe importante é que a função `compare` pode ser utilizada em qualquer tipo que seja instância da classe `Ord`, ou seja, que tenha uma relação de ordem definida. Por isso, é possível comparar não apenas datas, mas também outros tipos de dados, como números e strings.

## Veja também

- Documentação do módulo `Data.Ord`: https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Ord.html
- Documentação do módulo `Data.Time.Calendar`: https://hackage.haskell.org/package/time-1.11.1/docs/Data-Time-Calendar.html