---
title:                "Gleam: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Você já se perguntou como seria possível calcular uma data no futuro ou no passado? Existem diversas razões pelas quais alguém pode querer fazer esta operação, como prever datas importantes, organizar cronogramas ou até mesmo criar jogos que envolvem passagem do tempo. Independentemente do motivo, o Gleam torna este processo simples e fácil.

## How To

Para calcular uma data no futuro ou no passado usando o Gleam, basta utilizar a função `Date.add` passando como argumentos a data inicial e um valor inteiro representando a quantidade desejada de dias. Por exemplo:

```Gleam
let data_inicial = Date.new(2021, 10, 15)
let data_futura = Date.add(data_inicial, 365)
```

O código acima adiciona 365 dias à data inicial e armazena o resultado na variável `data_futura`. Você pode alterar o valor inteiro para uma quantidade maior ou menor de dias, dependendo da sua necessidade.

Para calcular uma data no passado, basta utilizar um valor inteiro negativo. Por exemplo:

```Gleam
let data_passada = Date.add(data_inicial, -15)
```

Este código subtrai 15 dias da data inicial e armazena o resultado na variável `data_passada`. Simples assim!

## Deep Dive

Na verdade, o Gleam permite que você faça muito mais do que apenas adicionar ou subtrair dias de uma data. Você também pode usar as funções `Date.add_months` e `Date.add_years` para adicionar meses e anos, respectivamente.

Além disso, o Gleam possui uma função `Date.diff` que calcula a diferença entre duas datas em dias. Este pode ser um recurso útil para quem deseja medir a passagem do tempo entre duas datas específicas.

## See Also

- [Documentação do Gleam sobre datas](https://gleam.run/modules/date)
- [Outros recursos úteis do Gleam](https://gleam.run/documentation)

Esperamos que este artigo tenha sido útil e que você possa usar o Gleam para calcular datas no futuro ou no passado de forma eficiente e fácil. Continue explorando todas as funcionalidades desta linguagem de programação funcional moderna e divirta-se criando aplicações incríveis!