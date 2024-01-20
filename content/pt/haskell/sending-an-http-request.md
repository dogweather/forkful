---
title:                "Enviando uma solicitação http"
html_title:           "Bash: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Haskell: Como enviar uma requisição HTTP

## O que & por quê?

O envio de uma requisição HTTP é o ato de solicitar dados a um servidor remoto através do protocolo HTTP. É uma necessidade comum na programação, principalmente no desenvolvimento web, onde é usado para obter ou enviar dados para APIs, sites e mais.

## Como:

Vamos usar o pacote `http-conduit` para facilitar nosso trabalho. Comece incluindo ele ao seu arquivo .cabal ou pacotes necessários, se estiver usando `stack`.

```Haskell
dependeries: ...
             - http-counit
```

Aqui está um exemplo simples sobre como enviar uma requisição GET:

```Haskell
import Network.HTTP.Conduit (simpleHttp)

main :: IO ()
main = do
  response <- simpleHttp "http://example.com"
  print response
```

Se você executar esse código, deverá ver o HTML da página `example.com` impressa na saída do console.

## Mergulhando Fundo

HTTP foi inventado por Sir Tim Berners-Lee em 1989 e tem sido um componente vital da web desde então. Alternativas existem, como o protocolo de transferência de hipertextos seguros (HTTPS), que é a versão segura do HTTP.
 
Quando consideramos detalhes de implementação, é importante salientar que `http-conduit` está usando a interface de entrada/saída do sistema de tipo abstrato (sockets). Ele implementa uma camada adicional sobre os chamados para fornecer uma interface fácil de usar para fazer solicitações HTTP.

## Veja Também

1. [http-conduit no Hackage](https://hackage.haskell.org/package/http-conduit)
2. [HTTP na Wikipédia](https://pt.wikipedia.org/wiki/Hypertext_Transfer_Protocol)

É isso aí! Agora você sabe como enviar uma solicitação HTTP em Haskell.