---
title:                "Elm: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que Comparar Duas Datas em Elm é Importante

Se você é um programador Elm, provavelmente já se encontrou em uma situação em que precisava comparar duas datas. Talvez você precise verificar se uma data é anterior ou posterior à outra, ou talvez precise encontrar a diferença entre elas. Independentemente do motivo, comparar datas é uma tarefa comum em muitos projetos Elm. Neste artigo, vamos explorar como fazer isso de forma eficiente e eficaz.

## Como Comparar Duas Datas em Elm

Felizmente, Elm possui uma biblioteca nativa chamada `Time` que fornece funções úteis para trabalhar com datas. Para comparar duas datas, podemos usar a função `Time.compare` que recebe duas datas e retorna um dos seguintes valores:

- `Time.Before` se a primeira data for anterior à segunda
- `Time.After` se a primeira data for posterior à segunda
- `Time.Same` se ambas as datas forem iguais

Vejamos um exemplo prático:

```Elm
import Time

data1 : Time.Posix
data2 : Time.Posix

-- Definindo as duas datas como timestamps do Unix (segundos desde 1º de janeiro de 1970)
data1 = 1577836800 -- 1º de janeiro de 2020 às 00:00:00
data2 = 1609459200 -- 1º de janeiro de 2021 às 00:00:00

-- Comparando as duas datas
comparação = Time.compare data1 data2

-- O resultado será Time.Before, já que a primeira data é anterior à segunda
```

Além disso, também podemos usar a função `Time.diff` para encontrar a diferença entre duas datas em unidades específicas, como dias, meses ou anos. Por exemplo:

```Elm
import Time

data1 : Time.Posix
data2 : Time.Posix

-- Definindo as duas datas como timestamps do Unix (segundos desde 1º de janeiro de 1970)
data1 = 1577836800 -- 1º de janeiro de 2020 às 00:00:00
data2 = 1609459200 -- 1º de janeiro de 2021 às 00:00:00

-- Encontrando a diferença em dias
diferençaEmDias = Time.diff Time.Days data1 data2

-- O resultado será 365, já que há exatamente 365 dias entre as duas datas
```

## Aprofundando na Comparação de Duas Datas

Ao comparar duas datas em Elm, é importante ter em mente que a precisão pode variar de acordo com o fuso horário do usuário. Por isso, é importante garantir que as datas estejam no mesmo fuso horário antes de fazer a comparação.

Outra dica útil é usar o sistema de tipos fortes do Elm para evitar erros de comparação de datas, como comparar uma data do tipo `Time.Posix` com uma data do tipo `Time.Extra.Calendar`. Usar tipos específicos para datas pode ajudar a prevenir esses erros e tornar seu código mais legível e robusto.

## Veja Também

- [Documentação da Biblioteca Time em Elm](https://package.elm-lang.org/packages/elm/time/latest/)
- [Exemplos de Comparação de Datas em Elm](https://gist.github.com/rtfeldman/98fcef1a9f1366913bc7e6f032904894)