---
title:                "Extraindo substrings"
html_title:           "Bash: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## O Que & Por Quê? 

Extrair substrings é o ato de remover uma parte específica de uma string maior. Programadores fazem isso para manipular dados, como criar resumos de texto ou extrair informações únicas de conjuntos maiores de dados.

## Como Fazer:

No Elm, você pode usar a função `String.slice`. Vamos ver um exemplo.

```Elm
import Html exposing (text)

substringExample : String
substringExample = 
    let
        initialString = "Olá, mundo!"
    in
    String.slice 0 5 initialString 

-- Saída: "Olá, "
```

## Mergulho Profundo 

O conceito de substrings remonta aos primórdios da programação, quando os computadores começaram a trabalhar com linguagens de texto. Em Elm, os índices das strings começam do 0, assim como na maioria das linguagens de programação.

Uma alternativa à função `String.slice` é a função `String.dropLeft` ou `String.dropRight`, que permitem que você descarte caracteres do início ou do fim de uma string.

Os detalhes de implementação das funções de substring no Elm são otimizados para melhor performance, aproveitando a estrutura de dados imutável das strings no Elm e a alocação eficiente de memória.

## Ver Também

A documentação oficial do Elm é sempre uma excelente fonte de informações:
- [String - Elm Packages](https://package.elm-lang.org/packages/elm/core/latest/String)
- [String.slice - Elm Packages](https://package.elm-lang.org/packages/elm/core/latest/String#slice)
- [String.dropLeft - Elm Packages](https://package.elm-lang.org/packages/elm/core/latest/String#dropLeft)
- [String.dropRight - Elm Packages](https://package.elm-lang.org/packages/elm/core/latest/String#dropRight)