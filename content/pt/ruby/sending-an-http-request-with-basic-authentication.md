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

## Por que

As solicitações HTTP são a base do tráfego na internet, permitindo que os usuários obtenham informações de servidores remotos. Quando deseja-se garantir que apenas usuários autorizados tenham acesso a essas informações, é necessário utilizar autenticação básica.

## Como fazer

Para enviar uma solicitação HTTP com autenticação básica em Ruby, primeiro é preciso criar um objeto `Net::HTTP`, que é responsável por fazer solicitações HTTP. Em seguida, é necessário definir os parâmetros da solicitação, incluindo a URL, método HTTP e cabeçalhos de autenticação. Por fim, é possível executar a solicitação com o método `request` e obter a resposta através do atributo `body`.

```Ruby
require 'net/http'

# Criando objeto Net::HTTP
url = URI("http://exemplo.com")
http = Net::HTTP.new(url.host, url.port)

# Definindo parâmetros da solicitação
request = Net::HTTP::Get.new(url)
request.add_field("Authorization", "Basic dXNlcm5hbWU6cGFzc3dvcmQ=")

# Executando solicitação e obtendo resposta
response = http.request(request)
puts response.body
```

## Aprofundando

Ao enviar uma solicitação HTTP com autenticação básica, é preciso codificar o nome de usuário e senha em uma string no formato "nomeDeUsuário:senha" e, em seguida, converter essa string para Base64. Isso garante que a informação seja transmitida de forma segura através da rede. Além disso, é importante lembrar que a autenticação básica não é considerada a forma mais segura de autenticação, pois a string Base64 pode ser facilmente decodificada, além do fato de que é necessário enviar as credenciais a cada solicitação, o que pode ser um problema em termos de desempenho.

Para saber mais sobre autenticação básica em Ruby, recomenda-se a leitura da documentação oficial do `Net::HTTP` e do módulo `Base64`. Também é importante considerar outras formas de autenticação mais seguras, como OAuth e Token-based authentication.

## Veja também

- Documentação oficial do `Net::HTTP`: https://docs.ruby-lang.org/pt-BR/2.7.0/Net/HTTP.html
- Documentação oficial do módulo `Base64`: https://docs.ruby-lang.org/pt-BR/2.7.0/Base64.html