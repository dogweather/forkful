---
title:                "Análise de uma data a partir de uma string."
html_title:           "Elm: Análise de uma data a partir de uma string."
simple_title:         "Análise de uma data a partir de uma string."
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O que é isso e por quê?
A conversão de uma data de string é um procedimento comum em programação em que uma data escrita como texto (por exemplo, "01/01/2022") é transformada em um formato de data mais adequado para cálculos e manipulações (por exemplo, 01 de janeiro de 2022). Isso é útil para realizar operações em data e hora, como comparação de datas, cálculos de diferença de tempo e exibição de informações no formato desejado.

## Como fazer:
```
Elm.DateTime.fromIso "2022-01-01" 
--> Ok (DateTime.fromDate 2022 1 1)

Elm.DateTime.fromString "01/01/2022 12:00 PM" 
--> Ok (DateTime.fromTime 12) 

Elm.DateTime.toTime "2022-01-01" 
--> Ok (Time.span 2022 0 1 0 0 0 0)
```

## Detalhes Complexos:
A conversão de datas é uma tarefa importante na programação, pois permite manipular e comparar datas de forma mais eficaz. Existem diversas bibliotecas e ferramentas disponíveis em Elm para facilitar o processo de conversão, como o pacote DateTime. No entanto, é importante ter cuidado com possíveis erros de formatação e conversão correta entre tipos de dados.

## Veja também:
- Documentação do pacote Elm DateTime: https://package.elm-lang.org/packages/elm/time/latest/
- Como trabalhar com datas em Elm: https://elmprogramming.com/dates-in-elm.html
- Conversão de datas em Elm: https://gist.github.com/mbs723/5d15c7ed3277e0260c8b0983e7562bf1