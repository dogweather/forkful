---
title:                "Elm: Calculando uma data no futuro ou passado"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Calculando uma data futura ou passada pode ser útil em diversas aplicações, desde a criação de um calendário até a determinação de prazos em projetos de software. Neste artigo, vamos explorar como realizar essa tarefa usando a linguagem de programação Elm.

## Como Fazer

Para calcular uma data no futuro ou passado em Elm, podemos utilizar a biblioteca `elm/time`. Primeiro, precisamos importar essa biblioteca no início do nosso código:
 
```
Elm import DateTime
```

Em seguida, podemos utilizar a função `DateTime.fromDate` para criar uma data a partir de valores específicos para ano, mês e dia:
```
Elm DateTime.fromDate 2021 12 31
```

Para calcular uma data no futuro ou passado, usamos a função `DateTime.add` e, como argumentos, passamos a unidade de tempo desejada (como anos, meses ou dias) e o número de unidades:
```
Elm DateTime.add DateTime.Month 3 (DateTime.fromDate 2021 12 31)
```
No exemplo acima, estamos adicionando 3 meses à data 31 de dezembro de 2021, resultando em 31 de março de 2022.

Podemos também calcular uma data no passado, passando um número negativo como o número de unidades:
```
Elm DateTime.add DateTime.Day -7 (DateTime.fromDate 2021 10 10)
```
No caso acima, estamos subtraindo 7 dias da data 10 de outubro de 2021, resultando em 3 de outubro de 2021.

## Profundidade

Ao usar a biblioteca `elm/time` para calcular datas no futuro ou passado, é importante ter em mente que ela leva em consideração os fusos horários. Portanto, é recomendável sempre fornecer o horário para evitar resultados inesperados.

Além disso, podemos utilizar outras funções da biblioteca, como `DateTime.toYear`, `DateTime.toMonth`, `DateTime.toDay` para extrair informações específicas de uma data e `DateTime.toTime` para converter uma data em um timestamp Unix (um número inteiro que representa a quantidade de segundos desde 1º de janeiro de 1970).

## Veja Também

- Documentação oficial da biblioteca `elm/time`: https://package.elm-lang.org/packages/elm/time/latest/
- Guia de Elm para iniciantes: https://guide.elm-lang.org/