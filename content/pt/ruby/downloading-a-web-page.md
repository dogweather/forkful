---
title:                "Fazendo o download de uma página da web"
html_title:           "Ruby: Fazendo o download de uma página da web"
simple_title:         "Fazendo o download de uma página da web"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O que e Porque?

Fazer o download de uma página da web significa obter o conteúdo de uma determinada página da internet e armazená-lo em seu computador. Os programadores muitas vezes fazem isso para obter informações relevantes, como dados de um site de notícias ou para automatizar tarefas repetitivas, como baixar imagens de um site.

## Como fazer:

```Ruby 
require 'open-uri'
 
page_content = open('https://exemplo.com').read
puts page_content
```

Ao executar esse código, você obterá o conteúdo da página em seu terminal. Lembre-se que o código pode variar dependendo do site e do que você está tentando obter.

## Mergulho Profundo:

Fazer o download de páginas da web é algo que os desenvolvedores têm feito por anos. Antes da existência da biblioteca open-uri, alguns métodos que eram usados incluíam Net::HTTP ou até mesmo o navegador de texto Lynx. No entanto, agora podemos usar a biblioteca open-uri para tornar esse processo muito mais fácil. Em alternativa, você também pode usar bibliotecas como Nokogiri para analisar o conteúdo da página da web em vez de apenas baixá-lo.

## Veja Também:

Para saber mais sobre como fazer o download de páginas da web em Ruby, confira a documentação oficial da biblioteca open-uri: https://ruby-doc.org/stdlib-2.7.0/libdoc/open-uri/rdoc/OpenURI.html