---
title:                "Convertendo uma data em uma string"
html_title:           "Haskell: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O que e Por que?

Conversão de uma data em uma string é uma operação comum na programação, que tem como objetivo transformar uma data em um formato legível e conveniente para o usuário. Programadores fazem isso para facilitar a visualização e manipulação de datas em seus códigos.

## Como fazer:

Conversão de data para string pode ser facilmente feita usando a função `show` do pacote `Data.Time`. Veja um exemplo:

```Haskell
import Data.Time (formatTime, defaultTimeLocale, Day)

dataFormat = "%d/%m/%Y" -- Formato desejado da data

main = do
  let myDate = fromGregorian 2021 4 23 -- Usando a função fromGregorian para criar uma data
  let strDate = formatTime defaultTimeLocale dateFormat myDate -- Convertendo para string
  putStrLn strDate -- Imprimindo a data formatada
```

Este código irá imprimir `23/04/2021` no console.

## Mergulhando Mais Profundo:

Historicamente, a conversão de data para string tem sido um desafio para programadores devido à variedade de formatos de data usados em diferentes países e culturas. Alguns dos formatos mais comuns são `dd/mm/yyyy` e `mm/dd/yyyy`. Além disso, existem várias bibliotecas e pacotes em Haskell que oferecem diferentes funções de conversão de data, como `Data.Time.Format` e `Data.Time.Clock.POSIX`, para citar alguns.

## Veja Também:

Para saber mais sobre a conversão de data para string em Haskell, você pode conferir a documentação do pacote `Data.Time` e também explorar outros pacotes relacionados que podem ser úteis em suas aplicações. Além disso, é sempre bom consultar a documentação oficial do Haskell para entender melhor os conceitos por trás desta linguagem funcional.