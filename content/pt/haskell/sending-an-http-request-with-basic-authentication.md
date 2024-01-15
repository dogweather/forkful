---
title:                "Enviando uma requisição http com autenticação básica"
html_title:           "Haskell: Enviando uma requisição http com autenticação básica"
simple_title:         "Enviando uma requisição http com autenticação básica"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por que

A autenticação básica é um método simples e amplamente utilizado para proteger informações confidenciais em pedidos HTTP. Ao enviar um pedido com autenticação básica, você garante que apenas usuários autorizados possam acessar os dados transmitidos pela solicitação.

## Como Fazer

Para enviar um pedido HTTP com autenticação básica em Haskell, primeiro precisamos importar o módulo "Network.HTTP.Simple" e o módulo "Data.ByteString.Char8", que nos permite trabalhar com strings de bytes.

```
import Network.HTTP.Simple
import Data.ByteString.Char8 (pack)
```

Em seguida, criamos as credenciais necessárias para a autenticação básica, que consistem em um nome de usuário e uma senha. É importante notar que a senha deve ser codificada em base64.

```
let username = "usuário"
let password = "senha"
let basicAuth = "Basic " ++ (pack $ username ++ ":" ++ password)
```

Agora podemos adicionar a autenticação básica ao nosso pedido HTTP, fornecendo o cabeçalho "Authorization" com o valor "basicAuth".

```
let request = setRequestHeader "Authorization" [basicAuth] "http://www.exemplo.com"
```

Finalmente, podemos enviar o pedido usando a função "httpBS" do módulo "Network.HTTP.Simple" e obter a resposta.

```
response <- httpBS request
```

Aqui está um exemplo completo de pedido HTTP com autenticação básica:

```
import Network.HTTP.Simple
import Data.ByteString.Char8 (pack)

main = do
    let username = "usuário"
    let password = "senha"
    let basicAuth = "Basic " ++ (pack $ username ++ ":" ++ password)
    let request = setRequestHeader "Authorization" [basicAuth] "http://www.exemplo.com"
    response <- httpBS request
    putStrLn $ "Resposta: " ++ (show $ getResponseBody response)
```

A saída seria algo como:

```
Resposta: "Conteúdo protegido"
```

## Mergulho Profundo

A autenticação básica é um método simples, mas não é considerado 100% seguro, pois a senha é transmitida em texto simples. Para aumentar a segurança, é recomendável usar a autenticação básica com uma conexão HTTPS.

Além disso, a autenticação básica pode ser facilmente interceptada e decodificada por um invasor, portanto, nunca deve ser usada para proteger informações altamente sensíveis.

## Veja Também

- [Documentação do módulo "Network.HTTP.Simple"](https://hackage.haskell.org/package/http-client-0.6.4/docs/Network-HTTP-Simple.html)
- [Documentação do módulo "Data.ByteString"](https://hackage.haskell.org/package/bytestring-0.10.12.0/docs/Data-ByteString.html)