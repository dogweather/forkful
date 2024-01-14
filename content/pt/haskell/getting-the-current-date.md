---
title:                "Haskell: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que usar Haskell para Obter a Data Atual

Se você é um programador experiente ou iniciante, provavelmente já passou pela tarefa de obter a data atual em algum momento. Em vez de recorrer aos métodos convencionais, por que não experimentar usar Haskell para realizar essa tarefa? Neste artigo, vamos explorar como obter a data atual em Haskell, e por que isso pode ser benéfico para os seus projetos.

## Como Obter a Data Atual em Haskell

Para obter a data atual em Haskell, usamos a biblioteca `Data.Time`. Dentro desta biblioteca, temos a função `getCurrentTime` que nos permite capturar a data e hora atuais em um formato específico. Veja um exemplo abaixo:

```Haskell
import Data.Time

main = do
  currentTime <- getCurrentTime
  print $ utcToLocalTime utc currentTime
```

Neste exemplo, usamos a função `utcToLocalTime` para converter o formato UTC (Universal Time Coordinated) em um formato de hora local mais fácil de ler. O resultado será algo como `2019-11-06 17:23:15.723726 UTC`.

## Aprofundando em Obtendo a Data Atual em Haskell

Além do exemplo mostrado acima, podemos personalizar ainda mais a saída da data atual em Haskell. Por exemplo, podemos usar a função `formatTime` para definir o formato de saída da data e hora. Veja um exemplo abaixo:

```Haskell
import Data.Time.Format
import Data.Time.Clock

main = do
  currentTime <- getCurrentTime
  let output = formatTime defaultTimeLocale "%d-%m-%Y %H:%M:%S" currentTime
  print output
```

Aqui, usamos a função `formatTime` juntamente com a variável `defaultTimeLocale` para formatar a data e hora no formato "dia-mês-ano hora:minuto:segundo". O resultado será algo como `06-11-2019 17:23:15`.

## Veja Também

- [Documentação oficial sobre Data.Time em Haskell](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Tutorial sobre formatação de datas e horas em Haskell](https://wiki.haskell.org/Local_time)
- [Exemplos práticos de uso de Data.Time em projetos reais](https://www.stackbuilders.com/tutorials/haskell/date-and-time/)