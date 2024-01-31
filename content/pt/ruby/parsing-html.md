---
title:                "Análise de HTML"
date:                  2024-01-20T15:33:28.439903-07:00
html_title:           "Bash: Análise de HTML"
simple_title:         "Análise de HTML"

category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?

Parsing HTML é o processo de transformar código HTML em uma estrutura compreensível para o programa manipular. Programadores fazem isso para extrair dados, manipular conteúdo ou até automatizar testes em páginas web.

## Como Fazer:

Para parsear HTML com Ruby, pode-se usar a gem Nokogiri, que é bem direta e poderosa. Instale-a com `gem install nokogiri`, e veja abaixo um exemplo básico:

```Ruby
require 'nokogiri'
require 'open-uri'

html = open('https://www.exemplo.com/')
doc = Nokogiri::HTML(html)

titulos = doc.css('h1')
titulos.each do |titulo|
  puts titulo.content
end
```

E a saída será algo assim, dependendo do HTML da página acessada:

```
Título no topo da página
Outro título importante
```

## Mergulho Profundo:

Historicamente, parsing de HTML era complexo e propenso a erros devido à natureza mal-estruturada de muitos documentos HTML. O Nokogiri é um dos melhores amigos de um programador Ruby para essa tarefa, pois fornece uma maneira simples porém robusta de navegar e manipular o DOM.

Alternativas ao Nokogiri são o Hpricot (descontinuado), que já foi popular, e bibliotecas baseadas em expressões regulares – mas cuidado, pois regex geralmente não é recomendado para parsear HTML devido à complexidade do mesmo.

Detalhes de implementação importantes: o Nokogiri pode se basear tanto em libxml2 quanto em nekoHTML para fazer o parsing, dependendo da configuração e do uso.

## Veja Também:

Para mais informações, aqui estão alguns links úteis:
- [Documentação Oficial do Nokogiri](https://nokogiri.org/)
- [Ruby Toolbox para HTML Parsing](https://www.ruby-toolbox.com/categories/html_parsing)
