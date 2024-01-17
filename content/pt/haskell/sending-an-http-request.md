---
title:                "Enviando uma solicitação http"
html_title:           "Haskell: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O Que e Por Que?

Enviar uma solicitação HTTP é um processo comum em programação, onde um programa envia uma mensagem a um servidor pela internet para obter dados ou realizar uma ação específica. Programadores fazem isso para interagir com APIs, acessar conteúdo de websites ou realizar comunicação entre sistemas.

## Como Fazer:

Haskell possui uma biblioteca padrão para realização de solicitações HTTP, chamada "HTTP". Para enviar uma solicitação GET a um determinado endereço URL, podemos usar a função `simpleHTTP`, que retorna uma `IO (Response String)`.

```Haskell
import Network.HTTP

main = do
    response <- simpleHTTP (getRequest "https://www.example.com")
    body <- getResponseBody response
    putStrLn body
```

Este código fará com que a resposta do servidor retorne como uma string no console.

## Deep Dive:

Enviar solicitações HTTP é uma parte fundamental do desenvolvimento de aplicações e websites modernos, pois permite a comunicação com recursos externos. Antes do surgimento da internet, desenvolvedores utilizavam outros protocolos, como o FTP, para realizar transferência de dados.

Uma alternativa ao uso da biblioteca padrão HTTP em Haskell é o uso do pacote "http-conduit", que possui uma API mais avançada e suporte a recursos como autenticação e SSL.

A implementação da biblioteca "HTTP" em Haskell utiliza um socket para estabelecer uma conexão TCP com o servidor e enviar a requisição HTTP. Depois de receber a resposta do servidor, a conexão é encerrada. 

## See Also:
- [Documentação da biblioteca HTTP em Haskell](https://hackage.haskell.org/package/HTTP)
- [Pacote "http-conduit" em Haskell](https://hackage.haskell.org/package/http-conduit)
- [Explicação sobre protocolo HTTP da W3Schools](https://www.w3schools.com/whatis/whatis_http.asp)