---
title:                "Ruby: Parsing html"
simple_title:         "Parsing html"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## Por que

O ato de analisar HTML é uma habilidade valiosa para qualquer programador Ruby. Ao analisar o código HTML de um site, você pode extrair informações importantes, como dados de tabelas ou textos de páginas específicas. Isso pode ser útil para tarefas como web scraping, onde você precisa extrair dados de vários sites.

## Como Fazer

Para realizar a análise de HTML em Ruby, você precisará de duas ferramentas principais: a gem Nokogiri e o método open-uri. Primeiro, instale a gem do Nokogiri utilizando o comando `gem install nokogiri` no seu terminal.

Em seguida, você pode utilizar o open-uri para abrir e ler a página web que deseja analisar. Por exemplo, se você deseja analisar a página inicial do Google, você pode usar o seguinte código:

```Ruby
require 'open-uri'
require 'nokogiri'

# Abrindo a página do Google
page = open("https://www.google.com")
# Lendo o conteúdo da página
html = page.read
```

A variável `html` agora conterá todo o código HTML da página do Google. Agora, você pode utilizar o Nokogiri para analisar esse código e extrair as informações desejadas. Por exemplo, se você deseja obter uma lista de todos os links da página, você pode usar o seguinte código:

```Ruby
require 'open-uri'
require 'nokogiri'

# Abrindo a página do Google
page = open("https://www.google.com")
# Lendo o conteúdo da página
html = page.read

# Utilizando o Nokogiri para analisar o HTML
parsed_html = Nokogiri::HTML(html)

# Obtendo todos os links da página
links = parsed_html.css('a').map{ |link| link['href'] }

# Imprimindo os links
puts links
```

Isso irá imprimir uma lista de todos os links presentes na página do Google.

## Deep Dive

Para uma análise mais detalhada de como o Nokogiri funciona, você pode verificar a documentação oficial da gem. Além disso, existem muitos tutoriais e guias disponíveis online que podem ajudá-lo a se aprofundar ainda mais na análise de HTML com Ruby.

Lembre-se de sempre verificar a estrutura do código HTML da página que você deseja analisar antes de começar a escrever seu código. Isso irá ajudá-lo a identificar quais elementos do HTML você precisa acessar e como pode extrair as informações desejadas.

## Veja Também

- [Documentação do Nokogiri](https://nokogiri.org)
- [Guia Prático para Análise de HTML com Ruby](https://www.sitepoint.com/web-scraping-ruby-nokogiri-mechanize/)
- [Tutorial de Web Scraping com Ruby e Nokogiri](https://www.digitalocean.com/community/tutorials/how-to-scrape-web-pages-with-ruby-and-nokogiri)