---
title:    "Elm: Escrevendo para o erro padrão"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever no erro padrão (Standard Error)

Escrever no erro padrão é uma prática comum em programação Elm. Isso nos permite lidar com erros de maneira mais eficiente e oferece uma maneira mais robusta de lidar com exceções em nosso código.

## Como fazer

Para escrever no erro padrão em Elm, podemos usar a função `Debug.crash` seguida por uma mensagem de erro entre aspas. Por exemplo:

```Elm
Debug.crash "Ocorreu um erro inesperado!"
```

Isso imprimirá a mensagem no console de desenvolvimento e ajudará a identificar a origem do erro.

Também podemos usar a função `Ergo.catch` para capturar e manipular erros específicos em nosso código. Por exemplo:

```Elm
import Ergo exposing (catch)

divide x y =
  if y == 0 then
    catch (Just "Não é possível dividir por zero") (\msg -> Debug.crash msg)
  else
    x / y
```

Este bloco de código primeiro verifica se o divisor é igual a zero e, se for o caso, chama a função `catch` para capturar o erro e passar uma mensagem personalizada para a função `Debug.crash`.

## Mergulho Profundo

Escrever no erro padrão em Elm também nos permite lidar com casos inesperados que podem surgir durante a execução do nosso programa. Podemos usar a função `Task.perform` para realizar uma tarefa e, se houver um erro, chamar a função `Debug.crash` para imprimir uma mensagem no console. Por exemplo:

```Elm
performTask =
  Task.perform
    (\err -> Debug.crash "Ocorreu um erro ao realizar a tarefa")
    (\success -> "Tarefa realizada com sucesso")
```

Esta função primeiro especifica o que deve ser feito em caso de erro (imprimir uma mensagem) e depois o que deve ser feito em caso de sucesso (retornar uma mensagem).

## Veja também

- Documentação do Elm: https://guide.elm-lang.org/
- Elm Weekly: https://elmlang.substack.com/
- Fórum da Comunidade Elm: https://discourse.elm-lang.org/