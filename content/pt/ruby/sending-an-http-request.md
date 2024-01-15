---
title:                "Enviando uma solicitação http"
html_title:           "Ruby: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Por que enviar uma solicitação HTTP é importante?

Enviar uma solicitação HTTP é essencial para interagir com a internet. Seja para acessar uma página da web, enviar dados para um servidor ou fazer uma chamada de API, todo esse processo é feito por meio de solicitações HTTP.

## Como fazer isso em Ruby?

Para enviar uma solicitação HTTP em Ruby, precisamos usar a biblioteca padrão Net::HTTP. Primeiro, precisamos criar um objeto de URL usando a URL do servidor que queremos acessar. Em seguida, criamos um objeto de solicitação HTTP especificando o tipo de solicitação, os cabeçalhos e os parâmetros necessários. Por fim, usamos o método `start` e `request` para enviar a solicitação e receber a resposta do servidor.

```Ruby
require 'net/http'

# Criando um objeto de URL
url = URI("https://example.com/api/users")

# Criando um objeto de solicitação HTTP
request = Net::HTTP::Post.new(url)
request['Content-Type'] = 'application/json'
request.body = { name: 'John', email: 'john@example.com' }.to_json

# Enviando a solicitação e recebendo a resposta
response = Net::HTTP.start(url.host, url.port, use_ssl: true) do |http|
  http.request(request)
end

puts response.body # Imprime a resposta do servidor
puts response.code # Imprime o status code da resposta
```

## Mergulho Profundo

Além do método `start` e `request`, a classe `Net::HTTP` possui outros métodos úteis para lidar com solicitações HTTP, como `get`, `post`, `put`, `delete`, entre outros. Também podemos configurar opções adicionais, como o timeout da solicitação ou o uso de um proxy. Além disso, podemos acessar informações do cabeçalho e corpo da resposta do servidor para tratar a resposta de acordo com nossas necessidades.

## Veja também

- Documentação Oficial do Net::HTTP: https://ruby-doc.org/stdlib-2.7.0/libdoc/net/http/rdoc/Net/HTTP.html
- Tutorial sobre o uso do Net::HTTP: https://www.rubyguides.com/2018/08/ruby-http-request/