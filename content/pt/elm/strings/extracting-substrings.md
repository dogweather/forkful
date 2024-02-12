---
title:                "Extraindo substrings"
aliases:
- /pt/elm/extracting-substrings/
date:                  2024-01-20T17:45:35.679302-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extraindo substrings"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Extrair substrings é o processo de pegar pedaços de uma string - como cortar um pedaço de torta para provar. Programadores fazem isso para manipular e usar partes específicas de textos, como dados de entrada, configurações ou até mesmo para análise de dados.

## Como Fazer:
```Elm
import String exposing (slice)

-- Vamos assumir que temos a seguinte string:
let
    texto = "Programação em Elm é elegante e robusta."
in
-- Para pegar a palavra "Elm":
String.slice 16 19 texto -- "Elm"

-- E se quisermos a frase "elegante e robusta"?
String.slice 22 39 texto -- "elegante e robusta"
```

Saída esperada:
```
"Elm"
"elegante e robusta"
```

## Mergulho Profundo
A extração de substrings não é algo novo; vem desde os tempos das primeiras linguagens de programação. Em Elm, utilizamos funções como `String.slice`, que é bem direta e faz o trabalho de maneira eficiente. Alternativas em outras linguagens incluem funções como `substring` ou métodos como `substr`. Mas, no Elm, ainda não temos uma função integrada chamada `substring`; `String.slice` é o padrão. A implementação de extrair substrings é uma funçao pura em Elm, ou seja, dadas as mesmas entradas, sempre terá a mesma saída, sem efeitos colaterais.

## Veja Também
- Documentação oficial da função `String.slice`: [Elm String.slice](https://package.elm-lang.org/packages/elm/core/latest/String#slice)
- Elm Lang: Uma introdução para iniciantes, onde substrings podem ser discutidas: [An Introduction to Elm](https://guide.elm-lang.org/)
