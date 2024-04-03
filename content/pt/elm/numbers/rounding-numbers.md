---
date: 2024-01-26 03:44:14.570125-07:00
description: "Como fazer: O m\xF3dulo `Basics` do Elm fornece fun\xE7\xF5es pr\xE1\
  ticas para arredondamento: `round`, `floor` e `ceiling`. Veja como us\xE1-las."
lastmod: '2024-03-13T22:44:46.494898-06:00'
model: gpt-4-0125-preview
summary: "O m\xF3dulo `Basics` do Elm fornece fun\xE7\xF5es pr\xE1ticas para arredondamento."
title: "Arredondamento de n\xFAmeros"
weight: 13
---

## Como fazer:
O módulo `Basics` do Elm fornece funções práticas para arredondamento: `round`, `floor` e `ceiling`. Veja como usá-las.

```elm
import Basics exposing (round, floor, ceiling)

-- Arredondar para o número inteiro mais próximo
round 3.14    --> 3
round 3.5     --> 4

-- Arredondar para baixo
floor 3.999   --> 3

-- Arredondar para cima
ceiling 3.001 --> 4

-- Truncar decimais sem arredondar
truncate 3.76 --> 3
```

O Elm também fornece `toLocaleString` para arredondar para um número fixo de casas decimais:

```elm
import Float exposing (toLocaleString)

-- Arredondar para duas casas decimais
toLocaleString 2 3.14159 --> "3.14"
```

## Aprofundando
Elm é uma linguagem funcional fortemente tipada que relega efeitos colaterais às "bordas" da arquitetura. Isso significa que funções como arredondamento devem ser puras e previsíveis. Historicamente, o arredondamento é uma operação comum em muitas linguagens de programação que lidam com a imprecisão da aritmética de ponto flutuante.

A abordagem do Elm para o arredondamento é direta - as funções são puras e aderem às definições matemáticas para round, floor e ceiling. Elm antecipa as necessidades comuns fornecendo funções integradas, pois a gestão da precisão é um requisito frequente, especialmente em finanças e gráficos.

Alternativas às funções integradas do Elm poderiam incluir implementações personalizadas usando operações aritméticas, mas isso adiciona complexidade desnecessária quando a biblioteca padrão já faz o trabalho de forma eficiente.

Na versão atual, o Elm utiliza a matemática de ponto flutuante subjacente do JavaScript para essas operações, mantendo-se assim consistente com o padrão IEEE 754, o que é algo a se lembrar ao considerar a precisão e possíveis erros de ponto flutuante.

## Veja Também
- Documentação oficial do módulo `Basics` do Elm: https://package.elm-lang.org/packages/elm/core/latest/Basics
- Um olhar detalhado sobre como funcionam os números de ponto flutuante na computação: https://floating-point-gui.de/
- Módulo `Float` do Elm para mais operações de ponto flutuante: https://package.elm-lang.org/packages/elm/core/latest/Float
