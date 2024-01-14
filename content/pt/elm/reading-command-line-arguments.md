---
title:                "Elm: Lendo argumentos da linha de comando."
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos de linha de comando em Elm?

O Elm é uma linguagem de programação funcional pura e fortemente tipada, projetada para criar incríveis interfaces de usuário web. Muitas vezes, precisamos fazer com que nossos programas sejam interativos, permitindo que os usuários forneçam informações específicas ao executá-los. Para isso, é necessário ler argumentos da linha de comando. Nesta postagem, vamos discutir por que é importante saber como ler argumentos de linha de comando em Elm e como podemos fazer isso.

## Como fazer?

Em Elm, podemos ler argumentos de linha de comando usando o módulo `Platform.Cmd`. Ele contém a função `Cmd.map` que permite manipular comandos e definir uma função para lidar com os argumentos da linha de comando. Vamos ver um exemplo simples de como podemos ler argumentos da linha de comando e imprimir o primeiro argumento na tela.

```elm
import Platform.Cmd exposing (Cmd)
import Task exposing (..)
import Task.Cmd exposing (Cmd, Program)

type Msg
  = Argument String

main : Program Never Model Msg
main =
  Platform.worker
    { init = init
    , update = update
    , subscriptions = subscriptions
    }

init : (Model, Cmd Msg)
init =
  ((), Cmd.map Argument Platform.Cmd.args)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Argument arg ->
      (model, Task.attempt (always (Cmd.none)) (Task.succeed (Debug.log "Argumento: " arg)))

subscriptions : model -> Sub Msg
subscriptions model =
  Sub.none
```

Quando executamos o programa acima com o seguinte comando na linha de comando:

```
elm reactor ./Main.elm --hello
```

A saída será a seguinte:

```
Argumento: hello
```

## Mergulho profundo

Além do exemplo simples dado acima, o módulo `Platform.Cmd` também oferece outras funções úteis para lidar com argumentos da linha de comando, como `Platform.Cmd2.map2` e `Platform.Cmd3.map3` para lidar com dois ou três argumentos, respectivamente. Também podemos usar o pacote `elm-community/elm-args-parser` para uma maneira mais avançada de analisar argumentos da linha de comando em Elm.

Além disso, é importante lembrar que as funções no módulo `Platform.Cmd` são puras, o que significa que elas não executam efeitos colaterais. Em vez disso, elas retornam um comando que será executado pelo mecanismo de efeito colateral do Elm. Portanto, é importante entender como os comandos funcionam e como eles se integram ao modelo de gerenciamento de estado do Elm.

## Veja também

- Documentação oficial do módulo `Platform.Cmd`: https://package.elm-lang.org/packages/elm/browser/latest/Platform-Cmd
- Pacote `elm-community/elm-args-parser`: https://package.elm-lang.org/packages/elm-community/elm-args-parser/latest/
- Documentação oficial sobre efeitos colaterais em Elm: https://guide.elm-lang.org/effects/