---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Clojure: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Enviar uma solicitação HTTP com autenticação básica é o processo pelo qual um cliente fornece credenciais, geralmente um nome de usuário e senha, ao servidor para ser autenticado. Os programadores fazem isso para garantir que apenas usuários autorizados acessem certos recursos ou funcionalidades.

## Como Fazer:
Em Haskell, podemos usar a biblioteca `http-conduit` para enviar solicitações HTTP. Para a autenticação básica, usamos `applyBasicAuth`. Veja o exemplo:

```Haskell
import Network.HTTP.Simple
import Network.HTTP.Client ()
import Data.ByteString.UTF8 (fromString)

main :: IO ()
main = do
    let username = fromString "usuario"
        password = fromString "senha"
        url = setRequestBasicAuth username password defaultRequest
               { getRequestMethod = "GET"
               , getRequestBody = RequestBodyBS "dados".toByteString
               }

    response <- httpBS url
    putStrLn $ getResponseBody response
```
Neste código, `usuario` e `senha` são suas credenciais, e `dados` é a informação que você deseja enviar.

## Deep Dive
Historicamente, a autenticação básica HTTP é usada há muito tempo. No entanto, ela é insegura por não criptografar as credenciais do usuário, sendo recomendado sempre usá-la com HTTPS.

Alternativas mais seguras incluem autenticação Digest e autenticação OAuth.

Em termos de implementação, `setRequestBasicAuth` funciona pelo cálculo da codificação Base64 das credenciais do usuário e depois alterando o cabeçalho de autorização do pedido para incluir essas credenciais codificadas.

## Veja Também
Interessado em aprender mais? Confira esses recursos:
- Documentação do http-conduit: https://www.stackage.org/haddock/lts-16.23/http-conduit-2.3.7.3/
- Discussão sobre autenticação básica VS OAuth: https://stackoverflow.com/questions/7565864/oauth-2-0-vs-basic-authentication
- Documentação Haskell ByteString: http://hackage.haskell.org/package/bytestring-0.10.8.2/docs/Data-ByteString.html