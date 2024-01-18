---
title:                "Analisando uma data de uma string"
html_title:           "Haskell: Analisando uma data de uma string"
simple_title:         "Analisando uma data de uma string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O que e Por Que?

Parsing de uma data a partir de uma string é o ato de extrair informações de uma string que representa uma data, e transformá-la em um formato de data estruturado. Programadores geralmente fazem isso para facilitar a manipulação e a comparação de datas em seus códigos.

## Como Fazer:

Para fazer o parsing de uma data a partir de uma string em Haskell, podemos usar a função `parseTimeM` do módulo `Data.Time.Format`. Por exemplo, se quisermos extrair uma data no formato "dia/mês/ano" da string "22/10/2021", podemos usar o seguinte código:

```Haskell
import Data.Time.Format
import Data.Time.Calendar

main = do
  let dateString = "22/10/2021"
  let date = parseTimeM True defaultTimeLocale "%d/%m/%Y" dateString :: Maybe Day
  print date
```
A saída seria: Just 2021-10-22, indicando que a string foi convertida para um valor do tipo `Day`.

## Profundando:

Para entender melhor o conceito de parsing de datas a partir de strings, é importante conhecer um pouco sobre a história das datas e calendários. Uma das primeiras formas de medir o tempo foi baseada nas fases da lua, e ao longo do tempo foram criados diversos calendários diferentes, como o calendário juliano e o calendário gregoriano.

Em termos de implementação, existem outras formas de fazer parsing de datas em Haskell, como usar a biblioteca `dateparser` ou escrever funções personalizadas para diferentes formatos de data. Além disso, é importante estar atento a questões como a localização (locale) e formatação da data, para garantir a correta conversão da string em data.

## Veja também:

- Documentação do módulo `Data.Time.Format`: https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html
- Documentação da biblioteca `dateparser`: https://hackage.haskell.org/package/dateparser
- História dos calendários: https://en.wikipedia.org/wiki/Calendar