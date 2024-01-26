---
title:                "Arredondamento de números"
date:                  2024-01-26T03:46:41.341480-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arredondamento de números"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/rounding-numbers.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Arredondar números significa aproximar um valor numérico a uma precisão específica, tipicamente para remover decimais indesejados. Programadores arredondam para gerenciar a memória, melhorar a legibilidade e atender a requisitos específicos do domínio, como restrições de moeda.

## Como fazer:

Swift oferece várias maneiras de arredondar números. Aqui está um exemplo:

```Swift
let original = 3.14159

// Arredondamento padrão
let standardRounded = round(original) // 3.0

// Arredondamento para um determinado número de casas decimais
let decimalRounded = Double(round(original * 1000) / 1000) // 3.142

// Arredondamento para baixo
let roundedDown = floor(original) // 3.0

// Arredondamento para cima
let roundedUp = ceil(original) // 4.0

print("Padrão: \(standardRounded), Decimal: \(decimalRounded), Para baixo: \(roundedDown), Para cima: \(roundedUp)")
```

Saída: `Padrão: 3.0, Decimal: 3.142, Para baixo: 3.0, Para cima: 4.0`

## Aprofundamento

Historicamente, o arredondamento é um conceito matemático que antecede os computadores, essencial no comércio e na ciência. O framework `Foundation` do Swift oferece funcionalidades abrangentes de arredondamento:

- `round(_: )` é o bom e velho arredondamento de meio para cima.
- `floor(_: )` e `ceil(_: )` lidam com o arredondamento direcional.
- `rounded(.up/.down/.toNearestOrAwayFromZero)` oferece um controle mais fino com um enum de regras de arredondamento.

Esteja ciente do tipo `Decimal` para cálculos financeiros precisos, o qual evita erros de ponto flutuante. Explore também `NSDecimalNumber` para compatibilidade com Objective-C.

## Veja também

- Padrão IEEE para Aritmética de Ponto Flutuante (IEEE 754): [IEEE 754](https://ieeexplore.ieee.org/document/4610935)