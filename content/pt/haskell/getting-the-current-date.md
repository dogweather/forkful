---
title:                "Haskell: Obtendo a data atual"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual em Haskell?

Pode parecer uma tarefa simples, mas obter a data atual em uma linguagem de programação pode ser muito útil em diferentes situações. Se você está desenvolvendo um aplicativo que requer informações de data e hora, ou precisa rastrear a duração de um processo, saber como obter a data atual em Haskell é essencial.

## Como fazer isso em Haskell

Em Haskell, existem diferentes maneiras de obter a data atual. A mais simples é usar a função `getCurrentTime` do módulo `Data.Time`, que retorna a data e hora atuais como valor do tipo `UTCTime`. Vamos ver um exemplo:

```Haskell
import Data.Time
import Data.Time.Format

main = do
    currentTime <- getCurrentTime
    putStrLn $ formatTime defaultTimeLocale "%d/%m/%Y, %H:%M:%S" currentTime
```

Neste exemplo, importamos os módulos `Data.Time` e `Data.Time.Format` para podermos usar a função `getCurrentTime` e a função `formatTime` para formatar a data de acordo com o nosso desejo. O resultado será algo como `16/04/2021, 18:00:00`, dependendo da data e hora atuais.

Outra forma de obter a data atual é usando o pacote `time`, que fornece funções mais avançadas para trabalhar com datas e horas. Vamos dar uma olhada em um exemplo usando o pacote `time`:

```Haskell
import Data.Time.Clock
import System.Locale (defaultTimeLocale)

main = do
    currentTime <- getCurrentTime
    putStrLn $ formatTime defaultTimeLocale "%d/%m/%Y, %H:%M:%S" currentTime
```

Como você pode ver, a diferença aqui é que importamos o módulo `Data.Time.Clock` e usamos a função `getCurrentTime` para obter a data atual.

## Mergulho profundo

O pacote `time` também fornece funções úteis para trabalhar com diferentes fusos horários, bem como funções para cálculos de duração entre datas e conversão de valores de tempo. Essas funções podem ser encontradas no módulo `Data.Time.Clock.HT` e são bastante úteis em situações em que é necessário manipular informações de data e hora de forma mais complexa.

## Veja também

- [Haskell.org](https://www.haskell.org/)
- [Documentação do módulo Data.Time](http://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Documentação do pacote time](http://hackage.haskell.org/package/time)