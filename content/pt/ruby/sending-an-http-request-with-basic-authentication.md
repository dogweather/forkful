---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Ruby: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O que & Porquê?
Enviar uma solicitação HTTP com autenticação básica é um processo comum em programação que permite aos desenvolvedores proteger suas APIs e outras aplicações web. Com a autenticação básica, é necessário fornecer um nome de usuário e senha para obter acesso aos recursos protegidos.

## Como fazer:
```Ruby
require 'net/http'

# Definir a URL para a qual queremos enviar a solicitação
url = URI("https://www.exemplo.com/api/usuarios")

# Criar um objeto de solicitação HTTP
request = Net::HTTP::Get.new(url)

# Adicionar informações de autenticação à solicitação
request.basic_auth('usuario', 'senha')

# Enviar a solicitação e receber a resposta
response = Net::HTTP.start(url.hostname, url.port, use_ssl: url.scheme == 'https') do |http|
  http.request(request)
end

# Imprimir o código de status da resposta
puts response.code

# Imprimir o corpo da resposta
puts response.body
```

## Deep Dive:
A autenticação básica é um método de autenticação criado em 1999 pelo World Wide Web Consortium (W3C). É amplamente utilizado por ser simples e fácil de implementar, mas também tem limitações de segurança, como o fato de que todas as informações são enviadas em texto puro. Existem outras formas mais seguras de autenticação, como a autenticação digest, que criptografa as informações de autenticação.

## Veja também:
- [Documentação do Ruby para Net::HTTP](https://ruby-doc.org/stdlib-2.7.0/libdoc/net/http/rdoc/Net/HTTP.html)
- [Tutorial sobre autenticação básica em Ruby](https://www.rubyguides.com/2018/08/ruby-http-basic-authentication/)