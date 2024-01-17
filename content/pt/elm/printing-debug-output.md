---
title:                "Imprimindo saída de depuração"
html_title:           "Elm: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## O que & Porquê?

Imprimir saída de depuração é simplesmente um método para os programadores verem os valores e etapas internas do código, para ajudá-los a entender o que está acontecendo em tempo de execução. É uma ferramenta útil para encontrar e corrigir erros no código e também pode ser usada para fins de aprendizagem e experimentação.

## Como fazer:

Usar a função `Debug.log` é a maneira mais comum de imprimir saída de depuração no Elm. É uma função de ordem superior que recebe uma mensagem de nome descritivo e um valor para imprimir. Aqui está um exemplo:

```
elm
import Debug exposing (log)

nome = "João"
idade = 30
log "Informações do usuário" (nome, idade)
```

O código acima irá imprimir a mensagem "Informações do usuário" e, em seguida, o valor dos dois atributos, `nome` e `idade`. A saída seria "Informações do usuário: (João, 30)".

## Profundidade:

Imprimir saída de depuração tem sido uma prática comum em programação por muitos anos e é usado em várias linguagens. No Elm, é especialmente útil por causa da forte tipagem e imutabilidade, o que permite aos programadores terem uma visão clara do estado do programa.

Existem também outras formas de depurar código no Elm, como usando a ferramenta Elm Reactor ou o navegador Elm Debugger.

A função `Debug.log` é implementada usando efeitos secundários, o que significa que ela não altera o código original. Isso garante que a saída de depuração só esteja presente em modo de desenvolvimento e não afete o desempenho do código em produção.

## Veja também:

- Documentação do Elm: https://guide.elm-lang.org
- Depuração em Elm com Elm Debugger: https://package.elm-lang.org/packages/elm/browser/latest/Browser-Debugger
- Elm Reactor: https://elm-lang.org/tools/reactor