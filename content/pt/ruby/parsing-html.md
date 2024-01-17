---
title:                "Analisando html"
html_title:           "Ruby: Analisando html"
simple_title:         "Analisando html"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## O que é e por que fazer?
Parsing HTML é o processo de analisar um documento HTML e extrair informações específicas dele. Programadores usam essa técnica para automatizar tarefas e manipular dados em páginas da web.

## Como fazer:
```Ruby
require 'nokogiri'
require 'open-uri'

# Fazendo a requisição para uma página web
html = open("https://www.example.com")
# Analisa o HTML da página
doc = Nokogiri::HTML(html)

# Extrai o texto do elemento <title>
puts doc.css("title").text

# Extrai todos os links da página
links = doc.css("a")
links.each do |link|
    puts link["href"]
end
```

## Aprofundando:
O parsing HTML é uma técnica fundamental na construção de aplicações web e robôs de web scraping. Existem outras ferramentas além do Nokogiri para fazer o parsing, como o Beautiful Soup em Python e o jsoup em Java. Além disso, é possível implementar parsers customizados usando bibliotecas como o Regex ou o XPath.

## Veja também:
- Documentação do Nokogiri: https://nokogiri.github.io/
- Tutorial de web scraping com Ruby e Nokogiri: https://www.rubyguides.com/2018/10/web-scraping-ruby-nokogiri/
- Alternativas ao Nokogiri: https://stackshare.io/nokogiri/alternatives