---
title:                "Haskell: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por que?
Downloading de páginas da web é uma habilidade fundamental para desenvolvedores Haskell que querem realizar operações de scraping, análise de dados ou criação de bots. Além disso, é uma tarefa bastante comum em projetos de aplicações web.

## Como fazer:
```Haskell
import Network.HTTP
import Network.URI

main = do
    let url = "https://www.example.com"
    document <- simpleHTTP (getRequest url) >>= getResponseBody
    print document
```

Nesse exemplo, usamos as bibliotecas `Network.HTTP` e `Network.URI` para fazer uma requisição HTTP e obter o conteúdo de uma página da web. Primeiro, usamos a função `getRequest` para criar uma requisição a partir de uma URL. Em seguida, usamos a função `simpleHTTP` para fazer a requisição, que retorna uma resposta. Por fim, usamos a `getResponseBody` para extrair o conteúdo da resposta.

O conteúdo obtido é uma string, que pode ser manipulada de diversas formas para extrair informações específicas ou realizar outras operações. Por exemplo, se queremos extrair todos os links presentes na página:

```Haskell
import Text.HTML.TagSoup

main = do
    let url = "https://www.example.com"
    document <- simpleHTTP (getRequest url) >>= getResponseBody
    let tags = parseTags document
    let links = filter isTagOpen links
    let hrefs = map (fromAttrib "href") links
    print hrefs
```

Nessa versão, utilizamos a biblioteca `Text.HTML.TagSoup` para fazer o parsing do conteúdo da página e extrair somente os links, que são identificados pelo atributo "href" dentro das tags `a`.

## Mergulho profundo:
Existem várias outras bibliotecas e técnicas para realizar o download de páginas da web em Haskell. Por exemplo, é possível utilizar a biblioteca `req` para fazer requisições HTTP de forma mais robusta, com suporte a TLS e outras configurações. Outra opção é utilizar o pacote `http-client-tls` para lidar com requisições HTTPS.

## Veja também:
- [Tutorial de requisições HTTP em Haskell](http://hackage.haskell.org/package/http-client-0.7.5/docs/Network-HTTP-Client.html)
- [Documentação da biblioteca TagSoup](http://hackage.haskell.org/package/tagsoup-0.14.7/docs/Text-HTML-TagSoup-Match.html)
- [Pacote req para requisições HTTP em Haskell](http://hackage.haskell.org/package/req-2.0.0/docs/Req.html)
- [Pacote http-client-tls para requisições HTTPS em Haskell](http://hackage.haskell.org/package/http-client-tls-0.3.5/docs/Network-HTTP-Client-TLS.html)