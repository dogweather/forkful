---
title:                "Elm: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Por que ler argumentos da linha de comando em Elm?

Se você é um programador Elm, pode se perguntar por que precisaria ler argumentos da linha de comando. A verdade é que, às vezes, pode ser útil na criação de programas interativos ou para obter informações de entrada do usuário em uma aplicação web. Por isso, é importante saber como ler argumentos da linha de comando em Elm.

## Como fazer

Em Elm, ler argumentos da linha de comando é bem simples. Tudo o que você precisa fazer é importar o módulo `Platform` e utilizar a função `Args.fromList`. Vamos ver um exemplo de código:

```Elm
import Platform
import Html exposing (text)

main =
  Platform.program
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

init flags =
  let
    args =
      Args.fromList flags.arguments
  in
  ( Model args, Cmd.none )

type alias Model =
  { args : Args.Arguments }

type Msg
  = NoOp

update msg model =
  case msg of
    NoOp ->
      ( model, Cmd.none )

view model =
  text ("Argumentos: " ++ toString model.args)

subscriptions _ =
  Sub.none
```

Neste exemplo, criamos uma aplicação basic que exibe os argumentos da linha de comando na tela utilizando a função `toString`. Para testar, você pode executar o comando `elm make Main.elm` no terminal e passar os argumentos desejados usando a flag `--`. Por exemplo, `elm make Main.elm -- --nome=fulano` irá exibir "Argumentos: nome=fulano" na tela.

## Aprofundando

Ao utilizar a função `Args.fromList`, você pode passar uma lista de strings para criar um `Arguments`, que contém os argumentos da linha de comando. Além disso, também é possível acessar argumentos específicos utilizando a função `get` e passando o índice desejado. Por exemplo, `model.args.get 0` irá retornar o primeiro argumento passado.

Outra função útil é o `getWithDefault`, que permite definir um valor padrão caso o argumento não seja encontrado. Por exemplo, `model.args.getWithDefault "nome" "Anônimo"` irá retornar o valor do argumento "nome" ou "Anônimo" caso o argumento não exista.

A leitura de argumentos da linha de comando em Elm pode ser particularmente útil na construção de aplicações mais complexas, como jogos ou aplicativos de linha de comando. Com essa funcionalidade, é possível obter informações do usuário sem a necessidade de formulários ou outros meios de entrada.

# Veja também

- [Documentação do módulo Platform.Args](https://package.elm-lang.org/packages/elm/core/latest/Platform-Args)
- [Tutorial sobre leitura de argumentos da linha de comando em Elm](https://medium.com/@dmorosinotto/reading-command-line-arguments-in-elm-622f1ba637e5)