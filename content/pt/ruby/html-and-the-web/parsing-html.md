---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:58.854865-07:00
description: "Analisar HTML significa desmembrar um peda\xE7o de c\xF3digo HTML para\
  \ compreender sua estrutura e conte\xFAdo. Os programadores fazem isso para extrair\
  \ dados,\u2026"
lastmod: '2024-03-11T00:14:20.837716-06:00'
model: gpt-4-0125-preview
summary: "Analisar HTML significa desmembrar um peda\xE7o de c\xF3digo HTML para compreender\
  \ sua estrutura e conte\xFAdo. Os programadores fazem isso para extrair dados,\u2026"
title: Analisando HTML
---

{{< edit_this_page >}}

## O Que & Por Que?
Analisar HTML significa desmembrar um pedaço de código HTML para compreender sua estrutura e conteúdo. Os programadores fazem isso para extrair dados, manipular conteúdo ou migrar informações entre formatos e sistemas.

## Como fazer:
Para analisar HTML em Ruby, instale a 'gema' Nokogiri com `gem install nokogiri`. Nokogiri é como um canivete suíço para trabalhar com HTML e XML em Ruby. Aqui está um exemplo rápido:

```ruby
require 'nokogiri'
require 'open-uri'

# Carregar o conteúdo HTML de um website
html_content = URI.open('http://example.com').read

# Analisar o HTML
doc = Nokogiri::HTML(html_content)

# Extrair o título
title = doc.xpath('//title').text
puts "O título da página é: #{title}"
```

Isso vai resultar em algo como: `O título da página é: Domínio de Exemplo`.

## Aprofundamento
Nos primeiros dias do Ruby, as opções para análise de HTML eram limitadas. REXML era embutido, mas lento. Então surgiu o Hpricot, mas ele acabou desaparecendo. Nokogiri estreou em 2008, combinando a facilidade do Hpricot com a velocidade e potência do libxml, um toolkit XML comprovado.

No mundo da análise, sempre existem alternativas. Alguns preferem a biblioteca embutida 'rexml' ou 'oga', outro analisador de XML/HTML para Ruby. Mas Nokogiri continua sendo o favorito por sua robustez e velocidade, sem mencionar sua vasta gama de recursos.

Por baixo dos panos, Nokogiri converte HTML em um Modelo de Objeto de Documento (DOM) — uma estrutura de árvore. Isso facilita a navegação e manipulação dos elementos. Usando XPath e seletores CSS, você pode identificar qualquer informação que precise.

## Veja Também
- Gema Nokogiri: [https://nokogiri.org/](https://nokogiri.org/)
- Documentação do rexml do Ruby: [https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html](https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html)
- Analisador alternativo 'oga': [https://github.com/YorickPeterse/oga](https://github.com/YorickPeterse/oga)
- Aprender sobre XPath: [https://www.w3schools.com/xml/xpath_intro.asp](https://www.w3schools.com/xml/xpath_intro.asp)
