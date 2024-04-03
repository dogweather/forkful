---
date: 2024-01-26 04:35:15.556873-07:00
description: "Como fazer: Vamos usar o REXML, inclu\xEDdo com Ruby, para fazer o parsing\
  \ de um snippet XML."
lastmod: '2024-03-13T22:44:47.119561-06:00'
model: gpt-4-0125-preview
summary: "Vamos usar o REXML, inclu\xEDdo com Ruby, para fazer o parsing de um snippet\
  \ XML."
title: Trabalhando com XML
weight: 40
---

## Como fazer:
Vamos usar o REXML, incluído com Ruby, para fazer o parsing de um snippet XML:
```Ruby
require 'rexml/document'
include REXML

xml_data = <<-XML
<frutas>
  <fruta nome="maçã" cor="verde"/>
  <fruta nome="banana" cor="amarelo"/>
</frutas>
XML

document = Document.new(xml_data)
document.elements.each('frutas/fruta') do |elemento|
  puts "Nome: #{elemento.attributes['nome']}, Cor: #{elemento.attributes['cor']}"
end
```
Saída:
```
Nome: maçã, Cor: verde
Nome: banana, Cor: amarelo
```

Gerar XML também é direto ao ponto:
```Ruby
doc = Document.new
doc.add_element 'frutas'
maca = doc.root.add_element 'fruta', {'nome' => 'maçã', 'cor' => 'verde'}
banana = doc.root.add_element 'fruta', {'nome' => 'banana', 'cor' => 'amarelo'}
puts doc
```
Saída XML:
```XML
<frutas>
  <fruta nome="maçã" cor="verde"/>
  <fruta nome="banana" cor="amarelo"/>
</frutas>
```

## Aprofundamento:
As raízes do XML remontam aos anos 1990 como um subconjunto simplificado do SGML para documentos web. É verboso, mas altamente estruturado, e é por isso que permaneceu. Não é a única opção — JSON e YAML tornaram-se populares por sua simplicidade — mas o XML se mantém forte em muitos sistemas empresariais e legados.

Ruby oferece algumas formas de lidar com XML. REXML é uma biblioteca toda em Ruby que é fácil de começar. Nokogiri é uma gem que envolve bibliotecas C mais rápidas, oferecendo velocidade e recursos extras. Escolher entre eles? Comece com o REXML para tarefas menores e passe para o Nokogiri se precisar de mais potência.

Por baixo dos panos, fazer o parsing de XML é sobre traduzir strings para modelos DOM ou SAX. DOM cria uma árvore na memória, enquanto SAX transmite o documento e dispara eventos à medida que faz o parsing. REXML oferece ambos os modelos, mas tende a ser mais lento do que extensões em C como as usadas pelo Nokogiri.

## Veja também:
- Documentação Ruby REXML: https://www.rubydoc.info/stdlib/rexml
- Gem Nokogiri: https://nokogiri.org/
- Especificação XML: https://www.w3.org/XML/
- Uma introdução ao SAX: https://www.saxproject.org/
- Comparação entre YAML vs. JSON vs. XML: https://www.upwork.com/resources/json-vs-xml-vs-yaml
