---
title:    "Elm: Escrevendo para o erro padrão"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão em Elm?

Escrever para o erro padrão em Elm pode ser uma tarefa útil para os desenvolvedores que desejam melhorar o gerenciamento de erros em seus programas. Ao escrever para o erro padrão, é possível obter uma visão mais clara dos erros que ocorrem durante a execução do código, facilitando a identificação e resolução de problemas.

## Como fazer

Para escrever para o erro padrão em Elm, é necessário utilizar a função `Debug.crash` que recebe uma mensagem de erro como parâmetro. Dentro dessa função, podemos chamar a função `toString` para converter qualquer tipo de dado em uma string e assim, exibi-lo como mensagem de erro. Veja um exemplo abaixo:

```elm
Debug.crash (toString "Ocorreu um erro inesperado!")
```

Este código irá imprimir a mensagem "Ocorreu um erro inesperado!" no erro padrão quando for executado.

## Profundidade técnica

Além de apenas exibir mensagens de erro, a função `Debug.crash` também pode ser utilizada para interromper a execução do programa. Isso significa que, quando ocorrer um erro, o programa irá parar de ser executado e as mensagens de erro serão exibidas no erro padrão.

Além disso, é possível utilizar o `Debug.todo` para marcar partes do código que ainda não foram implementadas, e assim, evitar erros durante a compilação.

## Veja também

- [Documentação oficial do Elm sobre Debug](https://guide.elm-lang.org/debugging/)
- [Artigo sobre gerenciamento de erros em Elm](https://www.elmprogramming.com/error-handling-in-elm.html)
- [Vídeo explicando o uso da função `Debug.crash`](https://www.youtube.com/watch?v=9OK-J1Gd-3M)