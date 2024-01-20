---
title:                "Comparando duas datas"
html_title:           "C#: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Comparar duas datas é medir a diferença entre elas em termos de dias, meses, anos etc. Programadores fazem isso para realizar cálculos de tempo, como o tempo restante para uma data, tempo decorrido desde um evento, e assim por diante.

## Como Fazer:

Vamos usar a biblioteca `Data.Time` no Haskell. Primeiro, você precisa importar a biblioteca:

```Haskell
import Data.Time
```
Depois de importar a biblioteca, você pode começar a comparar duas datas. Aqui está um exemplo:

```Haskell
import Data.Time
  
main = do
    let date1 = fromGregorian 2023 6 14 
    let date2 = fromGregorian 2023 7 16
    print $ diffDays date2 date1
```
A saída desse código seria:

```Haskell
32
```

## Mergulho Profundo

Historicamente, a comparação de datas foi introduzida à medida que os sistemas de computador começaram a exigir funcionalidades relacionadas à data e tempo, e as linguagens de programação, incluindo o Haskell, desenvolveram maneiras de manipular datas e tempo.

Existem múltiplas maneiras de comparar datas. Além do método `diffDays` mostrado acima, você também pode usar `diffUTCTime` ou `diffUTCTime` para comparar tempos de UTC.

A comparação de datas no Haskell é feita convertendo-as para Julian Day Number (um sistema contínuo de contagem de dias desde um passado remoto) e depois subtraindo uma da outra. Isso permite uma calculo eficiente da diferença, independentemente de como os calendários são estruturados.

## Veja Também

- [Data.Time](https://hackage.haskell.org/package/time-1.11.1.1/docs/Data-Time.html) documentação 
- [Comparando Datas](https://riptutorial.com/haskell/example/5805/date-comparison)
- [Julian Day](https://en.wikipedia.org/wiki/Julian_day) na Wikipedia