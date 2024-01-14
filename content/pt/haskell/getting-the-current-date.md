---
title:    "Haskell: Obtendo a data atual"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que

Hoje em dia, é comum lidar com datas em projetos de programação. Seja para exibir a data atual em uma página web ou para realizar cálculos com datas, é importante saber como obter a data atual em um programa. Felizmente, em Haskell, isso é muito simples de fazer e, neste post, vamos explorar como fazer isso.

## Como Fazer

Em Haskell, podemos obter a data atual usando a função `getCurrentTime` do módulo `Data.Time`. Para usá-la, primeiro precisamos importar o módulo:

```Haskell
import Data.Time
```

Em seguida, podemos chamar a função `getCurrentTime` para obter a data atual. Podemos armazenar esse valor em uma variável do tipo `UTCTime`, que representa um momento pontual sem fuso horário:

```Haskell
currentDate <- getCurrentTime
```

Podemos então manipular essa data usando funções do módulo `Data.Time.Format`, como `formatTime`, para formatá-la de acordo com nossas necessidades. Por exemplo, podemos exibir a data no formato "dd/mm/aaaa":

```Haskell
let formattedDate = formatTime defaultTimeLocale "%d/%m/%Y" currentDate
putStrLn formattedDate -- imprime "28/11/2021"
```

## Deep Dive

Além de `getCurrentTime`, o módulo `Data.Time` também possui outras funções úteis para trabalhar com datas, como `addUTCTime` para adicionar um tempo especificado a uma data, `diffUTCTime` para calcular a diferença entre duas datas e `toGregorian` para converter uma data do formato `UTCTime` para o formato `Day`, que representa um dia específico.

Além disso, é importante lembrar que a função `getCurrentTime` depende da data e hora do sistema em que o programa está sendo executado. Portanto, se for necessário trabalhar com uma data específica, é aconselhável usar o módulo `Data.Time.Calendar` e suas funções, como `fromGregorian` para criar uma data personalizada.

## Veja Também

- [Documentação oficial do módulo Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Tutorial de manipulação de datas em Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Simple%20date%20manipulation%20in%20Haskell)
- [Vídeo tutorial sobre manipulação de datas em Haskell](https://www.youtube.com/watch?v=Ei5e6kAkRfI)