---
title:                "Parseando HTML"
html_title:           "Ruby: Parseando HTML"
simple_title:         "Parseando HTML"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## Por que

Você pode se perguntar por que alguém se envolveria na análise de HTML. Bem, a capacidade de extrair informações de páginas da web é essencial para muitas tarefas de programação, como coletar dados para análise, criar crawlers de web e automatizar tarefas de visitas a sites.

## Como fazer

Embora existam muitas bibliotecas e ferramentas disponíveis para analisar HTML, o Ruby também possui uma ótima opção nativa chamada "Nokogiri". Veja como você pode usá-la para extrair informações de uma página HTML:

```Ruby
require 'nokogiri'
require 'open-uri'

page = Nokogiri::HTML(open("https://www.example.com"))

# Extrai o título da página
page.css('title').text

# Extrai todos os links da página
page.css('a').each do |link|
  puts link['href']
end
```

A saída será o título da página e todos os links encontrados. Você também pode usar seletores CSS para extrair elementos específicos ou até mesmo analisar páginas HTML aninhadas.

## Mergulho profundo

Nokogiri é uma biblioteca poderosa que suporta XPath, CSS e algumas outras opções para selecionar elementos em uma página HTML. Você também pode usar expressões regulares para encontrar padrões em tags ou texto.

Além disso, você pode usar a funcionalidade de traversing do Nokogiri para navegar facilmente entre os elementos HTML e acessar seus atributos e conteúdo.

## Veja também

- [Documentação do Nokogiri](https://nokogiri.org/)
- [Ruby on Rails: Como extrair dados de páginas da web](https://www.tutorialspoint.com/ruby-on-rails/rails-programs.htm)
- [Tutorial de análise de dados com Ruby](https://www.datacamp.com/community/tutorials/ruby-data-science)