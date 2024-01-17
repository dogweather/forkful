---
title:                "Baixando uma página da web."
html_title:           "Gleam: Baixando uma página da web."
simple_title:         "Baixando uma página da web."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Baixar uma página da web é o processo de transferir o conteúdo de uma página da internet para o seu computador. Os programadores geralmente fazem isso para acessar informações específicas contidas na página, que podem ser usadas em seus programas.

## Como fazer:
Para baixar uma página da web em Gleam, você pode usar a biblioteca "Fetch". Primeiro, importe a biblioteca em seu programa:
```Gleam
import gleam/fetch
```
Em seguida, use a função `get` para especificar a URL da página que deseja baixar e atribua o resultado a uma constante:
```Gleam
let page_content = fetch.get("https://example.com")
```
Por fim, você pode imprimir o conteúdo da página usando a função `stdout` e passando a constante como argumento:
```Gleam
stdout(page_content)
```
Este código irá imprimir todo o conteúdo da página, incluindo o código HTML e qualquer outro conteúdo presente nela.

## Mergulho profundo:
Baixar páginas da web é uma tarefa comum para muitos programadores, especialmente aqueles que trabalham com análise de dados ou web scraping. Existem muitas outras bibliotecas em Gleam que podem ser usadas para baixar páginas da web, como a biblioteca "HTTP". Além disso, é importante entender os protocolos HTTP e como o processo de download funciona para que você possa lidar com erros e respostas específicas do servidor.

## Veja também:
- Documentação oficial da biblioteca Fetch: https://gleam.run/modules/gleam/fetch.html
- Tutorial sobre como baixar páginas da web em Gleam: https://dennisreimann.de/articles/downloading-web-pages-in-gleam.html