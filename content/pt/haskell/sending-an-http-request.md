---
title:                "Enviando uma requisição HTTP"
date:                  2024-01-20T18:00:01.697981-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando uma requisição HTTP"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Enviar um pedido HTTP é basicamente pedir para um servidor web fazer algo ou dar algo. Programadores fazem isso para interagir com APIs, pegar dados, enviar informações, basicamente para conversar com a internet.

## Como Fazer:

Vamos usar a biblioteca `http-conduit` para enviar um pedido HTTP GET simples. Certifique-se de ter ela instalada usando `cabal install http-conduit`.

```Haskell
import Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpLBS "http://httpbin.org/get"
    putStrLn $ "O status code é: " ++ show (getResponseStatusCode response)
    putStrLn $ "O corpo da resposta é: " ++ show (getResponseBody response)
```

Quando você executa, a saída vai ser algo como:

```
O status code é: 200
O corpo da resposta é: "...alguns dados do servidor..."
```

## Mergulho Profundo

Enviar pedidos HTTP não é novidade. Na verdade, desde os primórdios da web, é o pão com manteiga da comunicação na internet. Alternativas para `http-conduit` incluem `wreq`, `req`, entre outras, cada uma com suas particularidades. O `http-conduit` emprega o conceito de streams para lidar com a resposta, o que pode ser um uso eficaz de memória para respostas grandes.

Internamente, um pedido HTTP é uma mensagem formatada enviada através do protocolo de comunicação TCP/IP. Em Haskell, abstraimos isso: fazemos uma chamada de função, e a biblioteca cuida dos detalhes internos pra gente.

## Veja Também

- Documentação da biblioteca `http-conduit`: https://hackage.haskell.org/package/http-conduit
- Guia HTTP para Haskell usando `http-client`: https://www.snoyman.com/tutorial/2017/01/30/http-client-in-haskell.html
- Comparação de bibliotecas de HTTP em Haskell: https://wiki.haskell.org/Http_libraries_comparison
