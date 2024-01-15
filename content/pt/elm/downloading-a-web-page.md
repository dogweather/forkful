---
title:                "Fazendo o download de uma página da web"
html_title:           "Elm: Fazendo o download de uma página da web"
simple_title:         "Fazendo o download de uma página da web"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por que

Se você está interessado em desenvolver páginas da web eficazes e eficientes, saber como fazer o download de uma página da web é essencial. Com a linguagem de programação Elm, você pode aprender a fazer isso de maneira simples e eficiente.

## Como Fazer

Para começar, é importante ter o Elm instalado no seu computador. Em seguida, você pode seguir os seguintes passos:

1. Importe o módulo `Http` para acessar as funções necessárias para fazer o download de uma página da web.
2. Crie uma função que utilize a função `send` do módulo `Http`. Esta função é responsável por fazer a requisição HTTP para o URL da página que você deseja fazer o download.
3. Atribua a função `send` a uma tarefa e utilize a função `Http.expectString` para especificar o tipo de resposta que você espera receber.
4. Use a função `Task.attempt` para lidar com a resposta da tarefa e convertê-la em um `Cmd Msg` que possa ser processado pela sua aplicação. Lembre-se de incluir esta `Cmd Msg` na função de atualização do seu modelo.
5. Para fazer o download da página, utilize a função `Http.send` na função `init` do seu programa.


```Elm
import Http
import String

type Msg = DownloadPage | ReceiveResponse (Result Http.Error String)

init : (Model, Cmd Msg)
init =
    ( initialModel, Http.send ReceiveResponse (Http.get "https://example.com") )
    
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        DownloadPage ->
            ( model, Http.send ReceiveResponse (Http.get "https://example.com") )
        
        ReceiveResponse (Result.Ok response) ->
            let
                pageContent =
                    response
                        |> String.lines
                        |> String.join " "
            in
                ( { model | page = pageContent }, Cmd.none )
                
        ReceiveResponse (Result.Err error) ->
            ( model, Cmd.none )
```

Com estes passos, sua aplicação será capaz de fazer o download de uma página da web e manipular seus dados como desejar.

## Profundidade

O módulo `Http` em Elm oferece várias funções e tipos de dados para lidar com requisições e respostas HTTP. Para uma maior compreensão sobre como essas funções e tipos trabalham juntos, é importante ler a documentação oficial do Elm sobre o módulo `Http` e experimentar com diferentes códigos e respostas.

Vale ressaltar que é importante ter conhecimento prévio sobre os conceitos básicos de requisições e respostas HTTP antes de tentar fazer o download de uma página da web em Elm.

## Veja Também

- Documentação oficial do módulo `Http` em Elm: https://package.elm-lang.org/packages/elm/http/latest/
- Um exemplo de aplicação que faz o download de uma página da web em Elm: https://github.com/elm/http/blob/0.19.1/examples/src/Characters.elm