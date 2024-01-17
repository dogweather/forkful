---
title:                "Parsing HTML"
html_title:           "Gleam: Parsing HTML"
simple_title:         "Parsing HTML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## O que e por que?

Fazer o parsing (análise) de HTML é essencial para manipular informações em páginas da web. Isso permite que os programadores extraiam dados específicos de uma página e os utilizem em seus aplicativos. Além disso, o parsing de HTML é necessário para fornecer uma experiência interativa ao usuário e realizar tarefas automatizadas em sites.

## Como fazer:

Para fazer o parsing de HTML no Gleam, você pode usar a biblioteca `html_parser`, que permite a extração de dados de uma página HTML de forma fácil e eficiente. Aqui está um exemplo de como utilizar essa biblioteca:

```
Gleam.import_html.parser

let html = "
<html>
  <body>
    <h1>Olá, Gleam!</h1>
    <p>Este é um exemplo de página HTML.</p>
  </body>
</html>
"

let doc = html_parser.parse(html)

let header = html_parser.find_one(doc, "h1")
let paragraph = html_parser.find_one(doc, "p")

```

O código acima importa a biblioteca `html_parser`, define uma variável com o conteúdo HTML, faz o parsing desse conteúdo e utiliza a função `find_one` para extrair o conteúdo do `h1` e do `p` da página.

## Explorando mais:

Apesar de ser uma tarefa comum no desenvolvimento web, o parsing de HTML pode ser um desafio para muitos programadores. Além disso, existem alternativas ao uso de bibliotecas externas, como o uso de expressões regulares, por exemplo. No entanto, a biblioteca `html_parser` do Gleam oferece uma implementação simples e robusta para essa tarefa.

## Veja também:

[Repositório do html_parser no GitHub](https://github.com/gleam-lang/html_parser)

[Tutorial sobre parsing de HTML em Gleam](https://gleam.run/news/html-parsing-tutorial/)