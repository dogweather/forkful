---
title:    "Elm: Cálculo de uma data no futuro ou no passado"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Por que

Calcular datas no futuro ou no passado pode ser útil em muitas situações, como em um programa de calendário ou em uma aplicação de controle financeiro.

## Como fazer

```Elm
-- Função para calcular uma data no futuro ou passado
calcularData : Int -> Int -> Date
calcularData dias tipo =
    if tipo == 1 then
        Date.init |> Date.addDays dias
    else
        Date.init |> Date.subDays dias
```

Exemplo de entrada: `calcularData 30 1`

Saída: `2021-08-05`

Exemplo de entrada: `calcularData 15 0`

Saída: `2021-07-20`

## Mergulho mais profundo

Calcular datas no futuro ou no passado pode ser feito de diversas maneiras, dependendo da linguagem de programação que você está usando. No caso de Elm, utilizamos a função `Date.addDays` para adicionar dias a uma data existente e `Date.subDays` para subtrair dias. Também é possível utilizar outras funções, como `Date.addMonths` e `Date.addYears`, para calcular datas em um período maior.

## Veja também

- https://guide.elm-lang.org/
- https://package.elm-lang.org/packages/elm/time/latest/