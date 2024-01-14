---
title:                "Haskell: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por que enviar uma solicitação HTTP?

Se você é um desenvolvedor Haskell em busca de uma maneira de se comunicar com servidores da web, o envio de solicitações HTTP é uma habilidade essencial. Isso permite que você faça chamadas para APIs, obtenha dados de URLs e envie informações para servidores remotos.

## Como fazer:

Aqui está um exemplo simples de envio de uma solicitação HTTP usando a biblioteca "http-conduit" em Haskell:

```Haskell
import Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpLBS "http://www.example.com"
    print $ getResponseBody response
```

Este código envia uma solicitação GET para o URL fornecido e imprime o corpo da resposta. Você também pode usar funções como "setRequestMethod" e "setRequestBody" para personalizar a sua solicitação com diferentes métodos (como POST, PUT, DELETE) e enviar dados com ela.

## Mergulho profundo:

Ao enviar uma solicitação HTTP, é importante entender os diferentes componentes envolvidos. Isso inclui o método da solicitação, a URL de destino, os cabeçalhos e o corpo da solicitação. Também é importante entender como lidar com erros e autenticação ao enviar solicitações HTTP.

## Veja também:

- Documentação da biblioteca "http-conduit": https://hackage.haskell.org/package/http-conduit
- Tutorial sobre como usar a biblioteca "http-client" em Haskell: https://stackoverflow.com/questions/21065786/how-do-i-make-a-simple-http-request-in-haskell