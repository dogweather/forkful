---
title:                "Haskell: Enviando uma requisição http com autenticação básica"
simple_title:         "Enviando uma requisição http com autenticação básica"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por que enviar uma solicitação HTTP com autenticação básica?

Enviar uma solicitação HTTP com autenticação básica é uma prática comum quando se trabalha com APIs e acesso a recursos protegidos. A autenticação básica fornece uma camada adicional de segurança, garantindo que apenas usuários autorizados possam acessar as informações protegidas.

## Como fazer

Para enviar uma solicitação HTTP com autenticação básica em Haskell, podemos utilizar a biblioteca `http-conduit`. Primeiro, precisamos importar o módulo `Network.HTTP.Simple`, que é responsável por gerenciar as solicitações HTTP.

Dentro de um bloco `do`, podemos utilizar a função `httpLbs` para fazer a solicitação. Precisamos fornecer a URL do recurso desejado e definir o método de autenticação como "Basic". Em seguida, precisamos fornecer o nome de usuário e senha para autenticar a solicitação. O código ficaria assim:

```Haskell
import Network.HTTP.Simple

main = do
    let url = "https://exemplo.com/recurso"
    request <- parseRequest url
    let auth = "Basic " ++ (encodeBase64 "usuario:senha")
    let request' = addRequestHeader "Authorization" auth request
    response <- httpLbs request'
    print $ getResponseBody response
```

O exemplo acima ilustra como fazer uma solicitação GET com autenticação básica. Se estivermos fazendo uma solicitação POST, também precisamos fornecer o corpo da solicitação usando a função `setRequestBodyJSON`. O resultado será uma resposta JSON que pode ser manipulada de acordo com as necessidades do nosso programa.

## Detalhes técnicos

A autenticação básica funciona enviando as credenciais do usuário codificadas em Base64 no cabeçalho "Authorization". É importante lembrar que isso não é uma forma segura de autenticação, pois as credenciais são facilmente decodificadas por alguém com conhecimento técnico. Por esse motivo, é recomendável usar outras formas de autenticação mais seguras, como OAuth.

## Veja também

- [Documentação do módulo Network.HTTP.Simple](https://hackage.haskell.org/package/http-conduit-2.2.4/docs/Network-HTTP-Simple.html)
- [Lista de bibliotecas de autenticação em Haskell](https://www.stackage.org/lts-16.31/search?q=auth) (em inglês)
- [Um exemplo mais avançado de autenticação básica em Haskell](https://stackoverflow.com/questions/36418076/haskell-http-client-auth-with-auth-basic-dont-think-it-is-sending-credentials) (em inglês)