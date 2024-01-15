---
title:                "Lendo argumentos da linha de comando"
html_title:           "Elm: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos de linha de comando em Elm?

Se você está familiarizado com a linguagem de programação Elm, provavelmente já sabe que é uma linguagem funcional que distribui código através da web. Mas o que muitas pessoas não sabem é que Elm também é capaz de ler argumentos de linha de comando. Isso pode ser extremamente útil em diferentes situações, como na criação de um programa que interage com o usuário ou na integração com outras ferramentas de linha de comando. Neste artigo, vamos explorar como ler argumentos de linha de comando em Elm e como isso pode facilitar o desenvolvimento de suas aplicações.

## Como fazer

A leitura de argumentos de linha de comando em Elm é feita através da função `Debug.todo`. Esta função permite que você passe uma mensagem e, em seguida, bloqueie sua aplicação, exibindo a mensagem no console do navegador. Veja um exemplo de como usar a função `Debug.todo` para ler um argumento de linha de comando:

```
Elm.Debug.todo "Testando argumentos de linha de comando"
```

Ao rodar este código, você deve ver a mensagem "Testando argumentos de linha de comando" aparecer no console do seu navegador. Agora, vamos ver como podemos usar essa função para realmente ler argumentos de linha de comando. Para isso, vamos criar uma função que recebe o argumento de linha de comando e o imprime no console:

```
import Platform exposing (Html)
import VirtualDom

type Msg
    = ReadArgs String


type alias Model =
    String


init : (String, Cmd Msg)
init =
    ( "Iniciando aplicação", Cmd.none )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ReadArgs arg ->
            ( "Argumento lido: " ++ arg, Cmd.none )


view : Model -> Html Msg
view model =
    VirtualDom.node "div" [] [ VirtualDom.text model ]


main : Program Never
main =
    Platform.program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
```

Neste exemplo, criamos uma função `update` que contém um caso de mensagem para a função `ReadArgs`. Em seguida, passamos essa função para a função `Platform.program`, que vai processar os argumentos de linha de comando e passá-los para a função `update`. No navegador, você pode testar isso passando um argumento de linha de comando após a URL, por exemplo: `suaaplicacao.com/?arg=abc`. Se você rodar esse exemplo, verá a mensagem "Argumento lido: abc" no console do navegador.

## Mergulhando fundo

Agora que sabemos como ler argumentos de linha de comando em Elm, vamos ver alguns detalhes importantes a serem considerados. Primeiro, a função `Platform.program` só funciona em navegadores, então se você precisa ler argumentos de linha de comando em outras plataformas como Node.js, você terá que encontrar uma biblioteca externa ou implementar sua própria solução.

Além disso, a função `Platform.program` também só é capaz de ler argumentos que seguem o padrão `nome=valor`, então se você precisar ler argumentos em formatos diferentes, você terá que modificá-los manualmente.

Outro detalhe a ser considerado é que a função `Platform.program` só é executada na inicialização da aplicação, então se você quiser ler argumentos em outros momentos durante a execução, você precisará recarregar a página com os argumentos correspondentes.

## Veja também

- [Documentação oficial do Elm sobre argumentos de linha de comando](https://package.elm-lang.org/packages/elm/core/latest/Platform#program)
- [Exemplo de implementação de leitura de argumentos de linha de comando em Node.js](https://erikdubbelboer.github.io/nodeJS/2013/06/22/parsing-gnu-getopt-arguments-in-node-js.html)
- [Outras funções úteis do módulo `Platform` do Elm](https://package.elm-lang.org/packages/elm/core/latest/Platform)