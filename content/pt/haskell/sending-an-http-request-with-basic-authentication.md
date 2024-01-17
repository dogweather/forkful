---
title:                "Enviar um pedido http com autenticação básica"
html_title:           "Haskell: Enviar um pedido http com autenticação básica"
simple_title:         "Enviar um pedido http com autenticação básica"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O que & Por que?
Enviar uma solicitação HTTP com autenticação básica é um processo pelo qual um aplicativo cliente pode acessar uma API protegida por nome de usuário e senha. Programadores fazem isso para garantir que apenas usuários autorizados tenham acesso aos dados da API.

## Como fazer:
```Haskell
import Network.HTTP
import Network.HTTP.Auth
import Network.HTTP.Headers
import Network.URI

main = do
  -- Criar a URI da API que será acessada
  let uri = parseURI "http://exemplo.com/api"
  -- Criar uma requisição HTTP GET
  let request = Request {rqURI = uri, rqMethod = GET, rqHeaders = [], rqBody = ""}
  -- Adicionar as credenciais de autenticação básica à requisição
  let request' = addBasicAuth "usuário" "senha" request
  -- Enviar a requisição e receber a resposta do servidor
  response <- simpleHTTP request'
  -- Obter o corpo da resposta e imprimir na tela
  let body = rspBody response
  putStrLn body
```

```
Output esperado:
Dados da API protegida por autenticação básica.
```

## Deep Dive:
A autenticação básica HTTP foi definida pela RFC 1945 em 1996 como parte do protocolo HTTP/1.0. Alternativas mais seguras, como o uso de chaves de API, tornaram-se mais populares, mas a autenticação básica ainda é amplamente utilizada, especialmente quando se trabalha com APIs legadas. A implementação acima utiliza a biblioteca HTTP da Haskell Platform, mas também existem outras bibliotecas que oferecem funcionalidades similares, como a wreq e a http-conduit.

## Veja também:
- [RFC 1945 - Protocolo HTTP/1.0](https://tools.ietf.org/html/rfc1945)
- [HTTP Library - Haskell Platform](https://hackage.haskell.org/package/HTTP)
- [wreq - Biblioteca para requisições HTTP em Haskell](https://hackage.haskell.org/package/wreq)
- [http-conduit - Biblioteca para requisições HTTP seguro e data streaming](https://hackage.haskell.org/package/http-conduit)