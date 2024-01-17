---
title:                "Baixando uma página da web"
html_title:           "Haskell: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

O que & Por quê?

Baixar uma página da web significa salvar o conteúdo de uma determinada página da web em seu próprio computador. Os programadores geralmente fazem isso para ter acesso offline ao conteúdo da página ou para extrair informações específicas dela.

Como fazer:

Para baixar uma página da web em Haskell, você pode usar a biblioteca "http-conduit". Aqui está um exemplo de código que baixa uma página e imprime seu conteúdo:

```
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L

main = do
  response <- simpleHttp "https://www.example.com/"
  print $ L.unpack response
```
Este código usará a função "simpleHttp" para fazer a solicitação HTTP e retornar uma resposta em formato de "ByteString". Em seguida, usando a função "print", podemos imprimir o conteúdo da resposta como uma "String".

Profundidade:

Historicamente, o processo de baixar uma página da web era feito manualmente, usando o protocolo "FTP" para transferir arquivos. No entanto, com o avanço da tecnologia, os programadores desenvolveram ferramentas e bibliotecas para automatizar esse processo. Além da biblioteca "http-conduit", também existem outras opções, como "Curl" e "wget", que podem ser usadas para baixar páginas da web em Haskell.

Vejo também:

- Documentação da biblioteca "http-conduit": https://hackage.haskell.org/package/http-conduit
- Outras opções para baixar páginas da web em Haskell: https://wiki.haskell.org/Downloading_web_pages