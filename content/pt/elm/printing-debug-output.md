---
title:                "Elm: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que imprimir saída de depuração no Elm?

Imprimir saída de depuração pode ser uma ferramenta útil para identificar e corrigir erros em seu código Elm. Ao visualizar as informações que estão sendo impressas, você pode entender melhor como seus dados estão sendo manipulados e encontrar possíveis problemas. Além disso, imprimir saída de depuração também pode ser útil para ver o fluxo de dados no seu programa, especialmente quando está lidando com dados complexos.

## Como fazer isso:

Para imprimir saída de depuração no Elm, você pode usar a função `Debug.log`, que recebe uma String e um valor como argumentos. A String é a mensagem que você deseja imprimir e o valor é o dado que você deseja ver. Veja um exemplo abaixo:

```elm
Debug.log "Meu número favorito é" 42
```

Este código irá imprimir "Meu número favorito é 42" na sua ferramenta de desenvolvimento, como o console do navegador, por exemplo.

## Mergulho Profundo:

Existem algumas coisas importantes a serem lembradas ao imprimir saída de depuração no Elm. Primeiro, é importante remover ou comentar todas as funções `Debug.log` no seu código final antes de implantá-lo na produção. Segundo, tenha cuidado ao imprimir valores muito grandes, pois isso pode tornar sua saída de depuração difícil de ler. Além disso, é possível usar `Debug.log` em qualquer lugar no seu código, dentro de funções, módulos ou até mesmo dentro de outras funções.

## Veja Também:

- [Documentação oficial do Elm sobre impressão de saída de depuração](https://elm-lang.org/docs/debugging)
- [Tutorial em vídeo sobre impressão de saída de depuração no Elm](https://www.youtube.com/watch?v=LvFN_B-KmJc)
- [Artigo sobre técnicas de depuração no Elm](https://medium.com/@stormcolour/functional-debugging-ftw-with-elm-4a07826e2a5a)