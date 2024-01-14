---
title:                "Ruby: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por que baixar uma página da web?

Baixar uma página da web pode ser útil por vários motivos, como por exemplo para realizar análises de dados, obter informações específicas ou simplesmente para fins de aprendizagem.

## Como fazer:

Para baixar uma página da web em Ruby, podemos usar a gem "open-uri", que permite abrir URLs como se fossem arquivos locais. Primeiro, precisamos instalar a gem em nosso ambiente Ruby:

```Ruby
gem install open-uri
```

Em seguida, podemos usar o método "open" para abrir a URL desejada e salvar o conteúdo em uma variável:

```Ruby
require 'open-uri'
url = "https://www.example.com"
page = open(url).read
```

Podemos então imprimir o conteúdo da página:

```Ruby
puts page
```

E já que o conteúdo foi salvo em uma variável, podemos também manipulá-lo da forma que for necessária.

## Mergulho profundo:

A gem "open-uri" também nos permite realizar operações mais avançadas ao baixar páginas da web. Por exemplo, podemos usá-la para realizar requisições com diferentes métodos HTTP, como GET, POST ou PUT. Também podemos adicionar headers personalizados ou até mesmo autenticar no servidor antes de baixar o conteúdo.

Além disso, podemos ler o conteúdo da página linha por linha ou até mesmo modificar as URLs dos links presentes na página antes de salvar o conteúdo. Tudo isso torna a gem "open-uri" uma ótima ferramenta para realizar tarefas relacionadas a páginas da web em nossos projetos em Ruby.

## Veja também:

- Documentação da gem "open-uri": https://ruby-doc.org/stdlib-2.6.3/libdoc/open-uri/rdoc/OpenURI.html
- Exemplos de uso da gem "open-uri": https://github.com/ruby/ruby/blob/master/lib/open-uri.rb
- Tutorial sobre como baixar e manipular páginas da web em Ruby: https://www.pluralsight.com/guides/ruby-web-scraping-and-processing-tutorial