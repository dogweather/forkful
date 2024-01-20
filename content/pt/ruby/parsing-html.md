---
title:                "Analisando HTML"
html_title:           "Arduino: Analisando HTML"
simple_title:         "Analisando HTML"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/parsing-html.md"
---

{{< edit_this_page >}}

---

## O Quê & Porquê?

Analisar HTML (HTML parsing) significa transformar o HTML em algum outro formato que seja mais fácil de trabalhar e interagir. Os programadores fazem isso para extrair informações úteis das páginas da web, manipular ou interactuar com essa informação de forma mais eficaz.

## Como fazer:

A seguir, um exemplo simples de como analisar HTML usando a biblioteca Nokogiri em Ruby.

```Ruby
require 'nokogiri'
require 'open-uri'

html = open("https://yourwebsite.com")
doc = Nokogiri::HTML(html)
titulos = doc.css('h1').map(&:text)

puts titulos
```

Supondo que seu site tem 2 títulos h1, a saída seria:

```Ruby
["Titulo 1", "Titulo 2"]
```

Esse código abre a página da web, analisa a informação HTML e extrai o texto dos títulos H1.

## Mergulho Profundo

Historicamente, a análise de HTML era muito mais trabalhosa antes das bibliotecas modernas simplificarem o processo. As alternativas ao Nokogiri no Ruby incluem Hpricot (agora descontinuado) e Oga. 

A biblioteca Nokogiri em particular faz uso de uma técnica chamada análise sintática em árvore (Tree parsing) para representar a estrutura de uma página da web de forma que possa ser facilmente navegada.

No exemplo de código acima, 'doc.css('h1')' retorna todos os nós h1 no documento como uma matriz.

## Ver Também

1. [Nokogiri Documentation](https://nokogiri.org/tutorials/parsing_an_html_xml_document.html)
2. [Ruby Open-URI Documentation](https://ruby-doc.org/stdlib-2.7.1/libdoc/open-uri/rdoc/OpenURI.html)
3. [Parsing HTML the Cthulhu Way](http://htmlparsing.com/ruby.html)

Sem "conclusão".