---
title:                "Baixando uma página da web"
html_title:           "Bash: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Descarregar uma página da web é o processo de copiar e guardar o conteúdo de um site no seu computador. Programadores fazem isso para backup, testar offline, análise de dados, e monitorar mudanças no conteúdo do site.

## Como Fazer:

Aqui está o código exemplo em Ruby para descarregar uma página da web usando a biblioteca `open-uri`:

```Ruby
require 'open-uri'

open('https://www.example.com') do |f|
  File.open('example.html', 'w') do |file|
    file.puts f.read
  end
end
```
O código acima irá descarregar o conteúdo da página 'https://www.example.com' e salvar localmente como 'example.html'.

## Imersão Profunda:

1. **Contexto Histórico**: A habilidade de baixar páginas da web se tornou vital com a ascensão da internet. Foi uma necessidade percebida por programadores para facilitar o processamento de dados e análise, levando ao desenvolvimento de diferentes bibliotecas e ferramentas. 
   
2. **Alternativas**: Existem muitas outras bibliotecas e ferramentas para descarregar páginas da web em Ruby, como `Net::HTTP`, `HTTParty`, e `Typhoeus`. Cada uma tem suas próprias características e benefícios, então explore e escolha a que melhor atenda às suas necessidades.

3. **Detalhes de Implementação**: A biblioteca `open-uri` em Ruby estabelece uma conexão com a URL fornecida, lê o conteúdo da página da web e o passa para o bloco. Dentro deste bloco, o conteúdo é escrito em um arquivo local. Este é um método simples para arquivos relativamente pequenos. Para arquivos maiores, ou se você precisa de mais controle sobre o processo, considerar usar uma biblioteca mais avançada.

## Veja Também:

- Documentação `open-uri`: https://ruby-doc.org/stdlib-2.7.1/libdoc/open-uri/rdoc/OpenURI.html
- Documentação `Net::HTTP`: https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html
- Gem `HTTParty`: https://rubygems.org/gems/httparty.
- Gem `Typhoeus`: https://rubygems.org/gems/typhoeus.