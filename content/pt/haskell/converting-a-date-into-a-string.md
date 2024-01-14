---
title:    "Haskell: Convertendo uma data em uma string"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data em uma string?

Ao escrever código em Haskell, é comum a necessidade de converter uma data em uma string. Isso pode ser útil para exibir datas em um formato mais legível para um usuário ou para armazenar datas em um formato específico em um banco de dados. Neste artigo, exploraremos como realizar essa conversão em Haskell.

## Como fazer:

Para converter uma data em uma string em Haskell, podemos utilizar a função `formatTime` do módulo `Data.Time.Format`. Esta função recebe dois argumentos: o primeiro é um formato de string e o segundo é a data que será convertida. Por exemplo, se quisermos converter a data atual em uma string no formato "Dia/Mês/Ano", podemos fazer o seguinte:

```Haskell
import Data.Time.Format
import Data.Time.Clock

main = do
    dataAtual <- getCurrentTime
    let dataString = formatTime defaultTimeLocale "%d/%m/%Y" dataAtual
    putStrLn dataString
```

O código acima utiliza a função `getCurrentTime` para obter a data atual e em seguida utiliza a função `formatTime` para converter essa data em uma string no formato desejado. O resultado será algo como "08/05/2021".

É importante notar que o formato da data é especificado utilizando códigos de formatação. No exemplo acima, "%d" representa o dia, "%m" representa o mês e "%Y" representa o ano. Podemos utilizar qualquer combinação desses códigos para criar o formato desejado.

## Aprofundando:

Uma coisa importante a se notar é que a função `formatTime` retorna uma `String` e não um `IO String`. Isso significa que não podemos utilizá-la diretamente em uma ação `do`. Podemos contornar esse problema utilizando a função `putStrLn` para exibir a string resultante na tela, como no exemplo anterior.

Além disso, é possível utilizar outros módulos como `Data.Time.Calendar` e `Data.Time.LocalTime` para trabalhar com diferentes tipos de data e hora em Haskell. Vale a pena explorar esses módulos e descobrir outras funcionalidades relacionadas a datas e strings.

## Veja também:

- [Documentação do módulo Data.Time.Format](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html)
- [Documentação do módulo Data.Time.Calendar](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Calendar.html)
- [Documentação do módulo Data.Time.LocalTime](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-LocalTime.html)