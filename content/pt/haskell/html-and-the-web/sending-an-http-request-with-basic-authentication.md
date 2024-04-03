---
date: 2024-01-20 18:01:47.344354-07:00
description: "Como Fazer: Vamos usar o pacote `http-conduit` para montar uma requisi\xE7\
  \xE3o HTTP com autentica\xE7\xE3o b\xE1sica em Haskell. Primeiro, instale o pacote\
  \ usando cabal."
lastmod: '2024-03-13T22:44:46.625052-06:00'
model: gpt-4-1106-preview
summary: "Vamos usar o pacote `http-conduit` para montar uma requisi\xE7\xE3o HTTP\
  \ com autentica\xE7\xE3o b\xE1sica em Haskell."
title: "Enviando uma requisi\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica"
weight: 45
---

## Como Fazer:
Vamos usar o pacote `http-conduit` para montar uma requisição HTTP com autenticação básica em Haskell. Primeiro, instale o pacote usando cabal:

```bash
cabal update
cabal install http-conduit
```

Agora, veja como montar sua requisição:

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Simple
import Network.HTTP.Types.Header (hAuthorization)
import Data.ByteString.Base64 (encode)
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
    let username = "user"
    let password = "pass"
    let auth = B.concat ["Basic ", encode $ B.concat [username, ":", password]]
    let request = setRequestHeader hAuthorization [auth] "http://yourapi.com/resource"
  
    response <- httpLBS request
    putStrLn $ "Status code: " ++ show (getResponseStatusCode response)
    print $ getResponseBody response
```

Saída de exemplo:

```
Status code: 200
"{\"data\":\"Some secure data\"}"
```

## Mergulho Profundo:
A autenticação básica HTTP é um método de autenticação web onde as credenciais do usuário (nome de usuário e senha) são codificadas em base64 e incluídas no cabeçalho da requisição. Embora seja simples e amplamente suportado, não é o mais seguro, pois se interceptado, os dados podem ser facilmente decodificados.

Alternativas modernas incluem autenticação baseada em tokens, como OAuth, que é mais segura e flexível. 

Quando implementamos requisições HTTP em Haskell, o `http-conduit` é frequentemente a escolha devido à sua simplicidade e poder. Ele permite customizar cabeçalhos, parâmetros, tipos de conteúdo e métodos de requisição, fornecendo um controle detalhado sobre as requisições HTTP.

## Veja Também:
Para saber mais sobre autenticação e segurança em APIs:

- Autenticação HTTP Básica: https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Headers/Authorization
- Haskell http-conduit: https://www.stackage.org/lts/package/http-conduit
- Autenticação OAuth: https://oauth.net/
