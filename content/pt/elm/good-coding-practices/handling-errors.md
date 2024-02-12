---
title:                "Tratamento de erros"
aliases: - /pt/elm/handling-errors.md
date:                  2024-01-26T00:52:20.221499-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tratamento de erros"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/handling-errors.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Tratar erros significa escrever código que pode antecipar e lidar com situações problemáticas. Os programadores fazem isso para prevenir falhas, proteger a integridade dos dados e fornecer aos usuários alternativas elegantes quando algo dá errado.

## Como fazer:
A filosofia central do Elm é Não Ter Exceções em Tempo de Execução. Dessa forma, o Elm tira proveito do seu sistema de tipos com tipos como `Maybe` e `Result` para gerenciar erros.

Para o cenário `Maybe`:

```Elm
safeDivide : Float -> Float -> Maybe Float
safeDivide numerator denominator =
    if denominator == 0 then
        Nothing
    else
        Just (numerator / denominator)
        
-- Quando você executa:

safeDivide 10 2
--> Just 5

safeDivide 10 0
--> Nothing
```

Para o cenário `Result`:

```Elm
type Error = DivisionByZero

safeDivide : Float -> Float -> Result Error Float
safeDivide numerator denominator =
    if denominator == 0 then
        Err DivisionByZero
    else
        Ok (numerator / denominator)

-- E usando:

safeDivide 10 2
--> Ok 5

safeDivide 10 0
--> Err DivisionByZero
```

## Aprofundando
O sistema de tipos do Elm é rigoroso, o que ajuda a detectar erros precocemente. Historicamente, a maioria das linguagens contava com exceções e verificações em tempo de execução, mas o Elm optou por garantias em tempo de compilação. Alternativas como `Result` permitem informações detalhadas de erros, enquanto `Maybe` é mais simples para cenários de sim ou não. O tratamento de erros em Elm encoraja os desenvolvedores a considerar todos os caminhos antecipadamente, evitando as armadilhas de casos de erro esquecidos.

## Veja Também:
- Seção do guia oficial do Elm sobre tratamento de erros: [Tratamento de Erros – Uma Introdução](https://guide.elm-lang.org/error_handling/)
- Documentação do Elm `Maybe`: [Elm – Maybe](https://package.elm-lang.org/packages/elm/core/latest/Maybe)
- Documentação do Elm `Result`: [Elm – Result](https://package.elm-lang.org/packages/elm/core/latest/Result)
