---
title:                "Imprimindo saída de debug"
html_title:           "C#: Imprimindo saída de debug"
simple_title:         "Imprimindo saída de debug"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## O Que & Por que?

Imprimir a saída de debug é uma técnica comum utilizada para rastrear e entender o fluxo de código e os valores das variáveis em tempo real. É útil para identificar e corrigir erros de lógica ou comportamento inesperado do código.

## Como Fazer:

Aqui está um exemplo do código Elm:

```Elm
import Html exposing (text)
import Debug

main =
  Debug.log "Valor de Debug " 42 |> text
```

Neste exemplo, `Debug.log` imprime "Valor de Debug: 42" na saída de depuração (como console do navegador, por exemplo) e o valor '42' é exibido em sua aplicação Elm.

## Mergulho Profundo

A função de depuração Elm `Debug.log` tem suas raízes nas funções semelhantes disponíveis em muitas outras linguagens de programação. Surgiu como uma maneira simples e rápida de depurar programas sem a necessidade de ferramentas de depuração mais complexas. No entanto, vale lembrar que a função `Debug.log` não deve ser usada em código final, ela serve como uma ferramenta de depuração temporária durante o desenvolvimento.

Além disso, existe também `Debug.todo`, que tem utilidade semelhante a `Debug.log`, mas é usado quando você sabe que parte do seu código ainda não está completa.

## Veja Também

Aqui estão alguns links úteis para saber mais sobre a saída de impressão de depuração em Elm:

- Guia Oficial do Elm: [Debug.log](https://package.elm-lang.org/packages/elm/core/latest/Debug#log)

- Guia do Usuário Elm: [Debugging](https://guide.elm-lang.org/effects/time.html)

- Artigo: [Debugging Elm Applications](https://www.divio.com/blog/debugging-elm-applications/)