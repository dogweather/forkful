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

##Por que
Você pode estar se perguntando por que alguém precisaria enviar uma solicitação HTTP em Haskell. Bem, o envio de solicitações HTTP é um processo essencial na comunicação entre aplicativos na web. É especialmente útil para realizar operações de leitura e gravação em bancos de dados, obter informações de servidores externos ou interagir com APIs.

##Como fazer
Aqui está um exemplo simples de como enviar uma solicitação HTTP em Haskell usando a biblioteca http-conduit:

````Haskell
-- Importando a biblioteca e criando uma função para enviar uma solicitação GET
import Network.HTTP.Conduit

getRequest :: String -> IO ()
getRequest url = do
    -- Criação do manager para gerenciar a conexão HTTP
    manager <- newManager tlsManagerSettings
    -- Criação da requisição GET
    request <- parseRequest url
    -- Envio da solicitação e armazenamento da resposta
    response <- httpLbs request manager
    -- Impressão do conteúdo da resposta
    print $ responseBody response
````

Com essa função, podemos enviar uma solicitação GET para qualquer URL e obter o conteúdo da resposta de volta. Por exemplo, podemos chamar `getRequest "https://www.google.com"` e obteremos o HTML da página inicial do Google.

Mas não é apenas para solicitações GET! Com a biblioteca http-conduit, podemos enviar solicitações HTTP de qualquer método (GET, POST, PUT, DELETE) e com vários cabeçalhos e parâmetros de consulta. Para aprender mais, confira a documentação dessa biblioteca incrível.

##Mergulho profundo
A biblioteca http-conduit é uma das várias bibliotecas disponíveis em Haskell para enviar solicitações HTTP. Ele utiliza o poderoso sistema de tipos da linguagem para criar uma interface simples e segura, garantindo que todos os dados sejam enviados e recebidos de maneira correta e eficiente.

Outra biblioteca popular para enviar solicitações HTTP em Haskell é a req. Ela oferece uma API intuitiva e funcional, e também é altamente configurável, permitindo que os usuários enviem solicitações HTTP com qualquer método, cabeçalhos personalizados e parâmetros de consulta.

Ambas as bibliotecas são amplamente utilizadas na comunidade Haskell e têm documentação abrangente e suporte ativo. Então, escolha a que melhor se adapta às suas necessidades e comece a enviar solicitações HTTP em Haskell de maneira fácil e elegante.

##Veja também
- Documentação da biblioteca http-conduit: https://hackage.haskell.org/package/http-conduit
- Documentação da biblioteca req: https://hackage.haskell.org/package/req
- Artigo sobre como enviar solicitações HTTP em Haskell usando a biblioteca wreq: https://mmhaskell.com/blog/2019/2/25/pretty-printing-an-http-response-in-haskell