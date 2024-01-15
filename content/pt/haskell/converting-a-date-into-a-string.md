---
title:                "Convertendo uma data em uma sequência de caracteres"
html_title:           "Haskell: Convertendo uma data em uma sequência de caracteres"
simple_title:         "Convertendo uma data em uma sequência de caracteres"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que

Você provavelmente está se perguntando: "Por que devo me preocupar em converter uma data em uma string?" Bem, se você está trabalhando com dados de datas em seu código, é importante saber como convertê-los em um formato legível para usuários ou para outras aplicações.

## Como Fazer

Para converter uma data em uma string em Haskell, podemos usar a função `formatTime` do módulo `Data.Time.Format`. Vamos dar uma olhada em um exemplo simples:

``` Haskell
import Data.Time.Format

dataString :: IO()
dataString = do
  let dataAtual = getCurrentTime
  let dataFormatada = formatTime defaultTimeLocale "%d/%m/%Y" dataAtual
  putStrLn dataFormatada
```

Neste exemplo, importamos o módulo necessário e criamos uma função chamada `dataString` que utiliza a função `getCurrentTime` para obter a data atual e a função `formatTime` para formatá-la em uma string no formato "dia/mês/ano". Em seguida, usamos a função `putStrLn` para imprimir a string resultante. O resultado será algo como "27/10/2021".

Existem diversas opções de formatação que podem ser utilizadas, como adicionar o dia da semana ou a hora da data. Para mais detalhes, consulte a documentação do módulo `Data.Time.Format`.

## Mergulho Profundo

Se você quiser aprofundar seus conhecimentos em como converter uma data em uma string em Haskell, é importante ter uma boa compreensão dos tipos de dados utilizados. Em Haskell, é comum trabalhar com tipos de dados como `UTCTime` (representa uma data e hora universal), `TimeZone` (representa um fuso horário) e `TimeLocale` (representa informações de localização na formatação de datas e horas).

Além disso, é útil estar familiarizado com outras funções relacionadas a datas e horas, como `parseTime` (para converter uma string em uma data) e `diffUTCTime` (para calcular a diferença entre duas datas).

## Veja Também

- [Documentação do módulo `Data.Time.Format`](https://hackage.haskell.org/package/time-1.12.0.2/docs/Data-Time-Format.html)
- [Tutorial Haskell do Wikibooks sobre o módulo `Data.Time.Format`](https://en.wikibooks.org/wiki/Haskell/FormatTime)
- [Post no blog Dev Community sobre como trabalhar com datas em Haskell](https://dev.to/chrisakwei/working-with-dates-and-time-in-haskell-ib4)