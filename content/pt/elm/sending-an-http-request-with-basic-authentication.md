---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Elm: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por que
Enviar solicitações HTTP com autenticação básica é essencial para acessar recursos protegidos em aplicativos e sistemas web. Ao fornecer um nome de usuário e senha, você pode autenticar sua identidade e obter acesso aos dados que precisa.

## Como Fazer
Para enviar uma solicitação HTTP com autenticação básica em Elm, você precisará usar a função `Http.send` e fornecer as informações necessárias. Aqui está um exemplo de código que envia uma solicitação GET com autenticação básica e lida com a resposta:

```
elm-package install krisajenkins/remotedata
```

```
import Http
import RemoteData exposing (..)

type Msg
    = GetData

type alias Model =
    { data : RemoteData Http.Error String
    }

init : ( Model, Cmd Msg )
init =
    ( { data = Loading }, Http.send GetData (Http.get "https://exemplo.com/dados" basicAuthConfig) )
    -- basicAuthConfig é um objeto com as chaves username e password fornecidas pelo usuário
    -- Se a solicitação for bem-sucedida, data será definido como Success com o corpo da resposta
    -- Se houver um erro, data será definido como Failure com informações sobre o erro

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetData ->
            ( model, Http.send GetData (Http.get "https://exemplo.com/dados" basicAuthConfig) )
```

Saída:
```
Success "Dados Protegidos"
```

## Mergulho Profundo
Ao enviar uma solicitação HTTP com autenticação básica em Elm, é importante garantir que sua conexão seja segura e seus dados protegidos. Certifique-se de usar o protocolo HTTPS em vez de HTTP para criptografar sua comunicação e reduzir o risco de ataques de interceptação. Além disso, é recomendável armazenar as credenciais de login em variáveis de ambiente ou em um arquivo de configuração externo, em vez de colocá-las diretamente no código.

## Veja Também
- Documentação oficial do Elm sobre o módulo Http: https://package.elm-lang.org/packages/elm/http/latest/
- Perguntas frequentes sobre autenticação básica HTTP: https://httpbin.org/#/Auth/get_basic_auth__user___passwd_