---
title:                "Exibindo saídas de depuração"
date:                  2024-01-20T17:52:19.930933-07:00
model:                 gpt-4-1106-preview
simple_title:         "Exibindo saídas de depuração"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## O Quê & Por Quê?
Imprimir saída de debug é como espiar no funcionamento do seu código. Programadores fazem isso para entender o que está acontecendo, encontrar bugs e garantir que tudo está nos trilhos.

## Como Fazer:
Vamos ver como isso funciona em Elm. Para imprimir algo para o console e ajudar no debug, você pode usar a função `Debug.log`. O seu programa continua funcionando normalmente após chamar esta função. Veja um exemplo:

```Elm
import Html exposing (text)
import Debug

main =
    let
        _ = Debug.log "Valor de x" 42
    in
    text "Cheque o console para ver a saída de debug!"

-- Saída no console:
-- "Valor de x: 42"
```

Quando você roda este pedaço de código, você verá "Valor de x: 42" no console do seu navegador.

## Aprofundando:
Agora, vamos mergulhar um pouco mais fundo. Elm tem um belo histórico de tornar a experiência de debug amigável. Desde o Elm 0.18, as ferramentas embutidas de debug como o Elm Debugger têm ajudado a inspecionar e retroceder o estado de aplicações Elm.

Alternativas ao `Debug.log` incluem usar o Elm Reactor para visualizações interativas de dados e o Elm Debugger para retroceder e avançar no tempo da sua aplicação, inspecionando como os dados mudam.

No entanto, `Debug.log` é essencial quando você precisa de algo rápido e fácil, especialmente durante o desenvolvimento inicial ou na investigação de problemas específicos que não exigem uma inspeção de estado inteiro.

É importante lembrar que as funções de debug não devem estar em código de produção. A comunidade do Elm valoriza a limpeza do código e a utilização de funções de debug apenas quando necessário, removendo-as antes de lançar o código ao mundo.

## Veja Também:
- Documentação Elm para Debugging: https://package.elm-lang.org/packages/elm/core/latest/Debug
- Elm Debugger walkthrough: https://elmprogramming.com/debugger.html
- Elm Reactor: https://guide.elm-lang.org/get_started.html#elm-reactor
