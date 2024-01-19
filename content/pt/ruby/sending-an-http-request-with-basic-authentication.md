---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Clojure: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O Que & Por Quê? 
Enviar uma solicitação HTTP com autenticação básica é um processo de enviar uma solicitação a um servidor da web com um nome de usuário e senha incorporados. Os programadores fazem isso para acessar APIs protegidas por senha e recuperar dados de forma segura.

## Como Fazer:
Para enviar uma solicitação HTTP com autenticação básica em Ruby, você pode usar a biblioteca `net/http`. Aqui está um exemplo simples:

```Ruby
require 'net/http'
require 'uri'

uri = URI.parse("http://exemplo.com")
http = Net::HTTP.new(uri.host, uri.port)
request = Net::HTTP::Get.new(uri.request_uri)
request.basic_auth("username", "password")
response = http.request(request)

puts response.body
```

Isso irá imprimir o corpo da resposta recebida.

## Mergulhando Mais Fundo
No contexto histórico, a autenticação básica foi introduzida nos primeiros dias da web. É simples e fácil de implementar, mas não é muito segura, pois nome de usuário e senha são enviados como texto simples.

Existem alternativas à autenticação básica, como a bearer token authentication e a OAuth, que são formas mais seguras de autenticação.

Ao enviar uma solicitação HTTP com autenticação básica, internamente, a biblioteca `net/http` codifica o nome de usuário e a senha em Base64 e adiciona esse token de autenticação ao cabeçalho da solicitação.

## Veja Também
Para informações mais aprofundadas, você pode consultar as seguintes fontes:

- [Documentação Oficial Ruby Net::HTTP](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html)
- [RFC7617: Autenticação básica HTTP](https://tools.ietf.org/html/rfc7617)
- [Autenticação Token Bearer](https://oauth.net/2/bearer-tokens/)
- [Guia de Autenticação OAuth](https://oauth.net/code/ruby/)