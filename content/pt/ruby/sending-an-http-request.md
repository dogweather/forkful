---
title:                "Enviando uma solicitação http"
html_title:           "Bash: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O que é e por quê?

Enviar um pedido HTTP é basicamente pedir a um servidor web informações ou ações. Os programadores fazem isso para interagir com APIs, recuperar informações, enviar dados, entre outras coisas.

## Como fazer:

Vamos usar a gem `net / http` para isso. Aqui está um exemplo de como enviar um GET request.

```Ruby
require 'net/http'
require 'uri'

uri = URI.parse("http://example.com")
response = Net::HTTP.get_response(uri)

puts response.body
```

Este script se conectará ao "http://example.com" e imprimirá a resposta.

Para enviar uma solicitação POST, podemos fazer:

```Ruby
require 'net/http'
require 'uri'

uri = URI.parse("http://example.com")
http = Net::HTTP.new(uri.host, uri.port)
request = Net::HTTP::Post.new(uri.request_uri)
response = http.request(request)

puts response.body
```

Este exemplo enviará uma solicitação POST a "http://example.com" e imprimirá a resposta.

## Mergulho Profundo

O protocolo HTTP foi desenvolvido no início da internet para permitir a comunicação entre clientes e servidores. A biblioteca `net / http` do Ruby, que estamos usando neste exemplo, está na linguagem desde o início.

Existem outras gems que você pode usar para enviar solicitações HTTP em Ruby, como 'httparty' e 'faraday'. Eles fornecem interfaces mais amigáveis e são mais fáceis de usar em alguns casos.

Por último, ao enviar uma solicitação HTTP, é importante lembrar de lidar com possíveis erros. Isso pode incluir verificar o status da resposta e implementar timeouts.

## Veja também

- Documentação 'net / http' do Ruby: https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html
- 'httparty': https://github.com/jnunemaker/httparty
- 'faraday': https://github.com/lostisland/faraday