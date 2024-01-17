---
title:                "Análise de HTML"
html_title:           "Elixir: Análise de HTML"
simple_title:         "Análise de HTML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/parsing-html.md"
---

{{< edit_this_page >}}

O que & Por quê?

A análise de HTML é o processo de extrair informações de um documento HTML. Os programadores fazem isso para automatizar o processo de obtenção de dados a partir de sites ou para processar configurações personalizadas para páginas da web.

## Como fazer:

### Analisando HTML usando a biblioteca Floki:

```elixir
html = "<html><body><h1>Título</h1><p>Parágrafo</p></body></html>"

Floki.find(html, "h1")
# output: "Título"

Floki.find(html, "p")
# output: "Parágrafo"
```

### Usando a biblioteca Meeseeks para analisar HTML de forma simplificada:

```elixir
html = "<html><body><a href="https://exemplo.com">Link</a></body></html>"

Meeseeks.cast_ex(html)
# output: "Link"
```

## Mergulho Profundo:

A análise de HTML é uma técnica importante para a extração de dados da web, mas pode ser difícil e demorado quando feita manualmente. Existem várias bibliotecas em Elixir que facilitam esse processo, como o Floki e Meeseeks mencionados anteriormente, além do HTMLParser e Beautiful Soup.

## Veja Também:

- [Documentação oficial do Floki](https://hexdocs.pm/floki/)
- [HTMLParser](https://hexdocs.pm/htmlparser/)
- [Beautiful Soup](https://hexdocs.pm/beautifulsoup/)