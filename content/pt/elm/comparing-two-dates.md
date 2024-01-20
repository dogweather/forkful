---
title:                "Comparando duas datas"
html_title:           "C#: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

---

## O Que & Por Quê?
Comparar duas datas é o processo de decidir qual data vem primeiro. Programadores fazem isso para ordenar eventos, identificar atrasos ou calcular a diferença de tempo entre eventos.

## Como Fazer:
Veja esse exemplo simples de como comparar duas datas no Elm:

```Elm
import Time

data1 : Time.Posix
data1 = Time.millisToPosix 1615895597000 

data2 : Time.Posix
data2 = Time.millisToPosix 1615890000000

compararDatas : Time.Posix -> Time.Posix -> Order
compararDatas data1 data2 = 
    compare data1 data2
```

A saída do código acima será `GT` se `data1` for maior que `data2`, `LT` se `data1` for menor que `data2`, ou `EQ` se as datas forem iguais.

## Mergulhe Fundo
Historicamente, a comparação de datas é uma necessidade comum em muitas aplicações. Antes de as funções built-in estarem disponíveis em muitas linguagens de programação, os desenvolvedores precisavam fazer cálculos manuais complexos considerando anos bissextos, fusos horários, etc.

Em Elm, você também pode usar a função `Time.diff` para obter a diferença de tempo entre as duas datas. No entanto, `compare` é geralmente mais conveniente quando você só precisa saber qual data é anterior, em vez da diferença exata de tempo.

## Veja Também
Para um guia completo sobre a manipulação de datas e horas no Elm, consulte a documentação oficial do Elm sobre o módulo Time [aqui](https://package.elm-lang.org/packages/elm/time/latest/Time).
Também não perca a fantástica série de blog posts do Charlton Roberts sobre a manipulação de datas no Elm [aqui](https://incrementalelm.com/how-do-i-model-time/).