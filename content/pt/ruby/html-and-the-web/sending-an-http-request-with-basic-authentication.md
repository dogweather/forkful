---
date: 2024-01-20 18:02:20.979987-07:00
description: "Como Fazer: A autentica\xE7\xE3o b\xE1sica HTTP existe desde os prim\xF3\
  rdios da web. \xC9 uma forma direta de controle de acesso, mas n\xE3o \xE9 segura\
  \ por padr\xE3o pois as\u2026"
lastmod: '2024-04-05T22:51:00.333002-06:00'
model: gpt-4-1106-preview
summary: "A autentica\xE7\xE3o b\xE1sica HTTP existe desde os prim\xF3rdios da web."
title: "Enviando uma requisi\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica"
weight: 45
---

## Como Fazer:
```Ruby
require 'net/http'
require 'uri'

uri = URI('http://exemplo.com/recurso_protetido')
req = Net::HTTP::Get.new(uri)
req.basic_auth 'usuario', 'senha'

resposta = Net::HTTP.start(uri.hostname, uri.port) {|http|
  http.request(req)
}

puts resposta.body
```
Saída esperada:
```
Conteúdo do recurso protegido.
```

## Aprofundamento
A autenticação básica HTTP existe desde os primórdios da web. É uma forma direta de controle de acesso, mas não é segura por padrão pois as credenciais podem ser facilmente decodificadas. Alternativas mais seguras como OAuth e tokens JWT surgiram para melhorar este aspecto.

Detalhes de implementação:
- As credenciais são codificadas em Base64 e passadas no cabeçalho 'Authorization' da seguinte forma: `Authorization: Basic <token>`.
- O método 'basic_auth' do Ruby simplifica esse processo.
- Não esqueça de usar HTTPS sempre que possível para proteger as credenciais durante a transmissão.

## Veja Também
- Documentação da classe Net::HTTP do Ruby: https://ruby-doc.org/stdlib/libdoc/net/http/rdoc/Net/HTTP.html
- Guia sobre autenticação HTTP básica da MDN: https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Authentication
- Informações sobre o método de codificação Base64: https://ruby-doc.org/stdlib/libdoc/base64/rdoc/Base64.html
- Descrição do protocolo OAuth: https://oauth.net/
