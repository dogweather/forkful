---
title:                "Obtendo a data atual"
html_title:           "Haskell: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que

Você sabia que é possível obter a data atual com apenas algumas linhas de código em Haskell? Isso pode ser útil para aplicações que requerem informações sobre o tempo, como sistemas de agendamento ou gerenciamento de tarefas.

## Como fazer

Usando a biblioteca "Data.Time", podemos facilmente obter a data e hora atuais em um formato específico. Aqui está um exemplo simples de como fazer isso:

```Haskell
import Data.Time

main = do
    now <- getCurrentTime
    putStrLn $ "A data atual é: " ++ show now
```

Este código primeiro importa o módulo "Data.Time", que contém funções relacionadas ao tempo. Em seguida, ele atribui a função "getCurrentTime" a variável "now", que representa a data e hora atuais. Por último, ele imprime essa informação na tela.

Ao executar esse programa, obtemos a seguinte saída:

```
A data atual é: 2021-10-26 14:23:48.356865 UTC
```

Podemos também formatar a data e hora de acordo com nossas preferências, usando a função "formatTime". Por exemplo:

```Haskell
import Data.Time
import System.Locale (defaultTimeLocale)

main = do
    now <- getCurrentTime
    let formattedTime = formatTime defaultTimeLocale "%d/%m/%Y - %H:%M" now
    putStrLn $ "Data e hora formatadas: " ++ formattedTime
```

A função "formatTime" recebe dois argumentos: o primeiro é um objeto Locale, que define as convenções de formatação a serem usadas, e o segundo é a data e hora que queremos formatar. Neste exemplo, usamos "defaultTimeLocale" para usar as convenções padrão do sistema e especificamos o formato desejado como "%d/%m/%Y - %H:%M", que resultará em uma saída como esta:

```
Data e hora formatadas: 26/10/2021 - 14:23
```

## Mergulho profundo

Você pode estar se perguntando como exatamente a biblioteca "Data.Time" é capaz de obter a data e hora atual. Na verdade, ela usa uma função interna chamada "getCurrentTime#", que usa funções do sistema operacional para recuperar a data e hora em UTC. Em seguida, ela a converte para um objeto "UTCTime", que é o tipo de dados usado para representar a data e hora em Haskell.

Além disso, a biblioteca "Data.Time" também possui funções para manipular e realizar cálculos com a data e hora, tornando-a uma ferramenta poderosa para lidar com questões relacionadas ao tempo em nossos programas.

## Veja também

- [Haskell.org - Data.Time](https://www.haskell.org/cabal/users-guide/developing-packages.html#additional-modules)
- [Hoogle - Data.Time](https://hoogle.haskell.org/?hoogle=Data.Time)
- [HaskellWiki - Dates and times](https://wiki.haskell.org/Dates_and_times)