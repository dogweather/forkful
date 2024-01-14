---
title:                "Haskell: Converter uma data em uma string"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que

Neste artigo, iremos explorar como converter uma data em uma string usando a linguagem de programação Haskell. Entender como fazer essa conversão pode ser útil para questões de formatação e apresentação de dados, especialmente em programas que envolvem manipulação de tempo.

## Como fazer

A conversão de data para string em Haskell pode ser feita usando a função `show` combinada com a estrutura de dados `UTCTime` da biblioteca `Data.Time`. Vamos dar uma olhada nesse exemplo simples:

```Haskell
import Data.Time

main = do
  let date = UTCTime (fromGregorian 2021 10 05) (secondsToDiffTime 0)
  putStrLn $ show date
```

A saída dessa execução será `2021-10-05 00:00:00 UTC`, em uma formatação padrão. Mas é possível customizar essa saída usando funções como `formatTime` e `defaultTimeLocale`. Por exemplo:

```Haskell
import Data.Time
import System.Locale

main = do
  let date = UTCTime (fromGregorian 2021 10 05) (secondsToDiffTime 0)
  putStrLn $ formatTime defaultTimeLocale "%d/%m/%Y" date
```

Agora, a saída será `05/10/2021`, em uma formatação de data mais comum no Brasil.

## Mergulho Profundo

Além do exemplo apresentado, existem diversas outras funções na biblioteca `Data.Time` que podem ser utilizadas para converter uma data em uma string. Por exemplo, a função `parseTimeM` permite fazer o processo inverso, ou seja, converter uma string em uma estrutura de data. Além disso, existem diversas opções de formatação que podem ser utilizadas em conjunto com a função `formatTime`.

É importante também entender que a conversão de data para string pode variar dependendo da localidade e das configurações de fuso horário do computador em que o programa está sendo executado. Por isso, é sempre recomendado verificar a documentação e realizar testes para garantir que a formatação está de acordo com as suas necessidades.

## Veja também

- [Documentação oficial do `Data.Time`](https://hackage.haskell.org/package/time-locale-compat-0.1.1/docs/Data-Time-Format.html)
- [Tutorial sobre conversão de data em Haskell](http://andrew.gibiansky.com/blog/haskell/haskell-dates.html)
- [Exemplo de aplicação da conversão de data em um programa real](https://wiki.haskell.org/Application_structure)