---
title:                "Baixando uma página da web"
html_title:           "Ruby: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por que

Fazer o download de uma página da web pode ser útil para extrair dados ou até mesmo para acessar informações offline.

## Como Fazer

Fazer o download de uma página da web é bastante simples usando a linguagem de programação Ruby. Primeiramente, é preciso instalar a gem "net/http", que irá permitir que nosso código faça requisições HTTP. Em seguida, podemos utilizar o método "get" desta gem passando a URL da página que desejamos fazer o download. Veja o exemplo abaixo:

```Ruby
require "net/http"

url = "https://www.mywebsite.com"

response = Net::HTTP.get(URI.parse(url))

puts response
```

Este código irá fazer o download da página "https://www.mywebsite.com" e imprimir o seu conteúdo no terminal. Podemos, então, utilizar o conteúdo da resposta para realizar outras tarefas, como armazenar em um arquivo ou fazer parsing para extrair informações específicas.

## Deep Dive

Ao fazer o download de uma página da web usando Ruby, é importante ter em mente que a resposta será fornecida em formato de string, o que significa que podemos utilizar todos os métodos de manipulação de strings disponíveis na linguagem. Além disso, é possível passar opções adicionais para o método "get", como o uso de autenticação ou passar headers específicos na requisição.

Outro ponto importante é entender que a resposta irá incluir não somente o conteúdo da página em si, mas também informações sobre o status da requisição, headers e outros metadados. Todas essas informações podem ser utilizadas para diferentes propósitos.

## Veja Também

- [Documentação da gem "net/http"](https://ruby-doc.org/stdlib-3.0.2/libdoc/net/http/rdoc/Net/HTTP.html)
- [Tutorial sobre Web Scraping com Ruby](https://www.geeksforgeeks.org/web-scraping-in-ruby-using-nokogiri-with-example/)