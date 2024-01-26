---
title:                "Enviando uma requisição HTTP com autenticação básica"
date:                  2024-01-20T18:02:20.979987-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando uma requisição HTTP com autenticação básica"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Enviar uma requisição HTTP com autenticação básica consiste em incluir credenciais de usuário e senha codificadas em Base64 no cabeçalho da requisição. Programadores fazem isso para acessar recursos na web que exigem identificação do usuário de forma simples, mas não totalmente segura.

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
