---
title:                "Ruby: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por que enviar uma solicitação HTTP com autenticação básica?

Enviar uma solicitação HTTP com autenticação básica permite aos desenvolvedores proteger endpoints e dados confidenciais em sua aplicação. Isso é especialmente importante ao lidar com informações sensíveis, como dados de usuário ou informações financeiras.

## Como fazer:

Para enviar uma solicitação HTTP com autenticação básica em Ruby, é necessário usar a biblioteca Net::HTTP. Primeiro, importe a biblioteca usando `require 'net/http'`. Em seguida, crie uma instância da classe Net::HTTP que corresponde ao URL da solicitação usando `Net::HTTP.new`. Por exemplo, para enviar uma solicitação para o endpoint `https://exemplo_api.com/endpoint`, use o seguinte código:
```Ruby
url = URI.parse("https://exemplo_api.com/endpoint")
http = Net::HTTP.new(url.host, url.port)
```

Em seguida, é necessário criar uma solicitação usando a classe Net::HTTP::Get e passar o URL da solicitação como argumento. Por exemplo:
```Ruby
request = Net::HTTP::Get.new(url)
```

Agora é hora de adicionar a autenticação básica à solicitação. Para isso, é necessário codificar o nome de usuário e senha em Base64. Em seguida, adicione a linha `request.basic_auth 'usuario', 'senha'`. O código completo deve ficar assim:
```Ruby
require 'net/http'
url = URI.parse("https://exemplo_api.com/endpoint")
http = Net::HTTP.new(url.host, url.port)
request = Net::HTTP::Get.new(url)
encoded_auth = "Basic " + Base64.encode64('usuario:senha')
request.add_field 'Authorization', encoded_auth
```

Por fim, envie a solicitação usando o método `http.request` e imprima a resposta usando `puts response.body`:
```Ruby
response = http.request(request)
puts response.body
```

## Mergulho Profundo:

Ao enviar uma solicitação HTTP com autenticação básica, é importante entender a estrutura da autenticação. A autenticação básica é um esquema de autenticação simples que envia o nome de usuário e senha como texto simples, codificado em Base64, em cada solicitação. No entanto, esse método é considerado não seguro, pois o cabeçalho da solicitação pode ser interceptado e decodificado facilmente.

Para tornar a autenticação básica mais segura, é recomendável usar HTTPS em vez de HTTP, pois os dados são criptografados durante a transmissão. Além disso, é importante gerar senhas fortes e alterá-las regularmente para evitar possíveis invasões.

## Veja também:

- [Documentação oficial do Ruby Net::HTTP] (https://ruby-doc.org/stdlib-2.6.3/libdoc/net/http/rdoc/Net/HTTP.html)
- [Explicação sobre autenticação básica em HTTP] (https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Basic_access_authentication)
- [Exemplo de código para enviar solicitações HTTP com autenticação básica em Ruby] (https://gist.github.com/jodosha/5923445)