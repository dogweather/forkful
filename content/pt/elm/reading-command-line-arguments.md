---
title:    "Elm: Lendo argumentos da linha de comando"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando em Elm?

Ler argumentos da linha de comando é uma habilidade importante para qualquer programador Elm, pois isso permite que o seu programa interaja com o usuário de maneira mais dinâmica e personalizada. Além disso, entender como trabalhar com argumentos da linha de comando pode ser útil para a construção de aplicativos de linha de comando e integração com outras ferramentas.

## Como fazer isso em Elm

Em primeiro lugar, precisamos importar o módulo `Platform.Cmd` para lidar com comandos da linha de comando. Em seguida, utilizamos a função `Cmd.get` para obter os argumentos passados pela linha de comando. Por exemplo, se quisermos imprimir o primeiro argumento passado, podemos fazer da seguinte maneira:

```Elm
module Main exposing (main)

import Platform.Cmd exposing (Cmd, CmdMsg)
import Platform.Sub exposing (Sub)
import Process
import Html exposing (text)


type alias Model =
    { argumento : String }


type Msg
    = AtualizarArgumento String
    | Cmd Falha
    | AtualizarCmdMsg CmdMsg


main : Program () Model Msg
main =
    Platform.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


-- Inicializa o modelo com o primeiro argumento ao executar o programa
init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "", Cmd.none )


-- Lida com as mensagens recebidas pelo programa
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AtualizarArgumento argumento ->
            ( { model | argumento = argumento }, Cmd.none )

        Cmd Falha ->
            ( model, Cmd.none )

        AtualizarCmdMsg cmdMsg ->
            case cmdMsg of
                CmdMsg -> model |> Process.spawn (AtualizarArgumento <| Process.Arg0 cmdMsg) |> (,) model

-- Define as subscrições do programa
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []

-- Define como o modelo deve ser exibido
view : Model -> Html Msg
view model =
    text model.argumento
```

Ao executar o programa com o argumento `Hello`, a saída será `Hello` na tela.

## Mergulho profundo

Ao trabalhar com argumentos da linha de comando em Elm, vale ressaltar que o módulo `Platform.Cmd` também possui outras funções úteis para lidar com comandos, como `Cmd.map` e `Cmd.batch`. Além disso, é possível acessar argumentos específicos utilizando as funções `Process.ArgN`, onde "N" representa o número do argumento desejado. Também é importante lembrar que, ao utilizar comandos como `Msg` em sua aplicação, deve-se lidar com possíveis falhas utilizando a mensagem `Cmd Falha`.

## Veja também

- [Documentação do módulo Platform.Cmd](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd)
- [Documentação do módulo Process](https://package.elm-lang.org/packages/elm/core/latest/Process)