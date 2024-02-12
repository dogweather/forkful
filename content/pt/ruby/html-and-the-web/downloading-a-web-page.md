---
title:                "Baixando uma página da web"
aliases: - /pt/ruby/downloading-a-web-page.md
date:                  2024-01-20T17:44:36.945852-07:00
model:                 gpt-4-1106-preview
simple_title:         "Baixando uma página da web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Baixar uma página da web é simplesmente o ato de obter o conteúdo HTML dessa página para seu uso local. Programadores fazem isso para análise de dados, monitoramento de conteúdo ou para automatizar interações com sites.

## Como Fazer:
Vamos usar a gema 'net/http' para baixar o conteúdo de uma página da web.

```Ruby
require 'net/http'
require 'uri'

# Definir o endereço web (URL) que queremos baixar
url = URI.parse('http://www.example.com')

# Uso do Net::HTTP para fazer a requisição GET
resposta = Net::HTTP.get_response(url)

# Imprimir o conteúdo do corpo (HTML) se a requisição foi bem-sucedida
puts resposta.body if resposta.is_a?(Net::HTTPSuccess)
```
Quando executar o código acima, você verá o HTML da página 'http://www.example.com' impresso no seu terminal.

## Aprofundamento
Historicamente, baixar páginas da web começou com scripts simples na linha de comando e evoluiu para frameworks sofisticados e bibliotecas em linguagens de programação. No Ruby, além da 'net/http', existem alternativas como a gema 'open-uri' para operações mais simples, ou 'Mechanize' e 'Nokogiri' para interações e parsing mais avançados. Na escolha das ferramentas, considere a complexidade da tarefa: 'net/http' é ótimo para operações básicas, mas se você pretende interagir com a página de formas mais complexas, olhe para 'Mechanize'.

## Veja Também
- [Ruby Doc Net::HTTP](https://ruby-doc.org/stdlib-3.1.0/libdoc/net/http/rdoc/Net/HTTP.html): A documentação oficial da gema 'net/http'.
- [Ruby Mechanize](https://github.com/sparklemotion/mechanize): Página no GitHub da gema 'Mechanize'.
- [Nokogiri](https://nokogiri.org/): Site oficial do Nokogiri, ótimo para parsing de HTML/XML.
