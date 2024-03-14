---
date: 2024-01-20 18:01:30.784809-07:00
description: "Enviar uma requisi\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica \xE9\
  \ o processo de acessar recursos protegidos em um servidor, fornecendo um nome de\
  \ usu\xE1rio e senha.\u2026"
lastmod: '2024-03-13T22:44:46.499772-06:00'
model: gpt-4-1106-preview
summary: "Enviar uma requisi\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica \xE9 o\
  \ processo de acessar recursos protegidos em um servidor, fornecendo um nome de\
  \ usu\xE1rio e senha.\u2026"
title: "Enviando uma requisi\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica"
---

{{< edit_this_page >}}

## What & Why?
Enviar uma requisição HTTP com autenticação básica é o processo de acessar recursos protegidos em um servidor, fornecendo um nome de usuário e senha. Programadores fazem isso para garantir que apenas usuários autorizados acessem certos dados.

## How to:
```Elm
import Http
import Base64

type alias Credentials =
    { username : String
    , password : String
    }

-- Encode your username and password
encodeCredentials : Credentials -> String
encodeCredentials creds =
    "Basic " ++ (Base64.encode (creds.username ++ ":" ++ creds.password))

-- Create the Authorization header
authorizationHeader : Credentials -> Http.Header
authorizationHeader creds =
    Http.header "Authorization" (encodeCredentials creds)

-- Send an HTTP request with Basic Auth
sendRequestWithBasicAuth : Credentials -> String -> (Result Http.Error String -> msg) -> Cmd msg
sendRequestWithBasicAuth creds url toMsg =
    Http.request
        { method = "GET"
        , headers = [ authorizationHeader creds ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectString toMsg
        , timeout = Nothing
        , tracker = Nothing
        }

-- Exemplo de uso
creds = Credentials "user" "password"
url = "https://some-protected-resource.com"
```

## Deep Dive
Autenticação básica é um método antigo, introduzido pelo HTTP/1.0. Apesar de sua simplicidade, hoje em dia é considerada insegura se usada sem HTTPS, pois as credenciais são enviadas como texto puro codificado em Base64. Alternativas modernas incluem OAuth e tokens JWT. No Elm, a autenticação básica é realizada adicionando o cabeçalho de autorização a uma requisição. Vale notar que o módulo `Http` do Elm gerencia os detalhes de baixo nível da rede e o `Base64` garante a codificação correta das credenciais.

## See Also
- [Elm HTTP package documentation](https://package.elm-lang.org/packages/elm/http/latest/)
- [Base64 Encoding in Elm](https://package.elm-lang.org/packages/truqu/elm-base64/latest/)
- [OAuth 2.0](https://oauth.net/2/)
- [JSON Web Tokens (JWT)](https://jwt.io/)
