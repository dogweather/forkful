---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Clojure: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Enviar uma solicitação HTTP com autenticação básica significa fornecer nome de usuário e senha em um formato codificado para validar sua sessão. Programadores fazem isso para proteger recursos online dos usuários e evitar acessos não autorizados.

## Como Fazer:
Aqui estão algumas etapas simples para enviar uma solicitação HTTP com autenticação básica em Elm.

```Elm
module Main exposing (..)

import Http
import Json.Decode as Decode

type Msg
    = GotUser (Result Http.Error String)

getUser : Cmd Msg
getUser =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" "Basic = QWxhZGRpbjpvcGVuIHNlc2FtZQ==" ]
        , url = "https://api.seuwebsite.com/users/1"
        , body = Http.emptyBody
        , expect = Http.expectString GotUser
        , timeout = Nothing
        , tracker = Nothing
        }
```

No exemplo acima, `QWxhZGRpbjpvcGVuIHNlc2FtZQ==` é o nome de usuário e senha codificados.

## Mergulho Profundo:

- **Contexto Histórico:** A autenticação básica HTTP é um método de autenticação que permite a um cliente HTTP fornecer um nome de usuário e senha quando faz uma solicitação. Foi projetado em 1996 e continua sendo uma maneira popular de autenticação por sua simplicidade.

- **Alternativas:** Além da autenticação básica, existem outras formas de autenticação, como o OAuth e o token JWT. Estes oferecem um nível de segurança superior e são recomendados para aplicações modernas.

- **Detalhes de Implementação:** Na autenticação básica HTTP, o nome de usuário e a senha são concatenados com um dois-pontos (:) como separador, e então a string resultante é codificada em Base64.

## Veja Também:
1. [Elm HTTP package documentation](https://package.elm-lang.org/packages/elm/http/latest)
2. [The Basics of HTTP Basic Authentication](https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Authentication)
3. [Alternative method: OAuth](https://oauth.net/)
4. [Alternative method: JWT](https://jwt.io/introduction/)