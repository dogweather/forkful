---
title:                "Elm: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever no padrão de erro?

Escrever no padrão de erro é uma prática comum em programação que permite ao desenvolvedor capturar e exibir mensagens de erro para ajudar a identificar e solucionar problemas em seu código. É uma maneira útil de depurar e aprimorar seu programa, tornando-o mais robusto e confiável.

## Como fazer

Para escrever no padrão de erro em Elm, você pode usar a função `Debug.crash` que recebe uma mensagem de erro como argumento. Por exemplo:

```Elm
import Debug

Debug.crash "Houve um erro aqui!"
```

Isso exibirá uma mensagem de erro personalizada no console do navegador. Você também pode adicionar valores de variáveis ​​à sua mensagem de erro para ajudar a rastrear problemas específicos em seu código. Por exemplo:

```Elm
import Debug

let
  num1 = 10
  num2 = 0
  result = num1 / num2
in
  Debug.crash ("Não é possível dividir " ++ toString num1 ++ " por " ++ toString num2)
```

Isso exibirá uma mensagem de erro indicando o motivo pelo qual a operação de divisão falhou.

## Aprofundando

No Elm, é possível personalizar ainda mais as mensagens de erro, adicionando identificadores de tipo e de linha ao seu código. Isso pode ser útil quando você tem muitas funções e deseja saber em qual delas ocorreu o erro. Por exemplo:

```Elm
import Debug

let
  num1 = 10
  num2 = 0
  result = num1 / num2

foo: Int -> Int
foo x =
  Debug.crashWithId "Divisão" x ("Não é possível dividir por 0")

bar: Int
bar =
  foo result
```

Isso exibirá uma mensagem de erro com o nome da função (`foo`) e o número da linha em que o erro ocorreu.

## Veja também

- [Documentação oficial do Elm sobre escrever no padrão de erro](https://guide.elm-lang.org/error_handling/debugging.html#crash-functions)
- [Artigo sobre como rastrear e depurar erros em Elm](https://www.elm-tutorial.org/en/05-resources/02-debugging.html#crash-and-referrer-functions)
- [Vídeo tutorial sobre depuração de erros em Elm](https://www.youtube.com/watch?v=uxUdsNSlyZA)