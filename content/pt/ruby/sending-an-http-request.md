---
title:                "Ruby: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por que Enviar uma Solicitação HTTP no Ruby

Enviar uma solicitação HTTP no Ruby é uma habilidade importante para qualquer desenvolvedor que esteja trabalhando com aplicações web. Com essa funcionalidade, é possível fazer uma comunicação entre diferentes servidores e utilizar dados de outras fontes.

## Como Fazer

Para enviar uma solicitação HTTP no Ruby, é preciso utilizar a biblioteca padrão Net::HTTP. Primeiro, é necessário criar uma instância da classe Net::HTTP, passando como argumento a URL do servidor que se deseja acessar.

```Ruby
require 'net/http'

url = URI("https://www.example.com")
http = Net::HTTP.new(url.host, url.port)
```

Em seguida, é preciso definir o tipo de solicitação que será feita, como GET, POST, PUT, DELETE, entre outros. Além disso, é possível enviar parâmetros ou dados junto com a solicitação usando o método `set_form_data`.

```Ruby
request = Net::HTTP::Get.new(url)
request.set_form_data({"nome" => "João", "sobrenome" => "Silva"})
```

Para finalizar, basta executar a solicitação usando o método `request` e salvar a resposta em uma variável. No exemplo abaixo, a resposta é armazenada na variável `response` e seus dados são impressos no terminal.

```Ruby
response = http.request(request)
puts response.body
```

## Mergulho Profundo

Existem diversas opções de configuração que podem ser utilizadas ao enviar uma solicitação HTTP no Ruby. É possível, por exemplo, definir o tempo limite da solicitação, escolher o tipo de conexão (HTTP ou HTTPS), adicionar cabeçalhos personalizados e muito mais. Além disso, é importante lembrar de tratar possíveis erros de conexão ou resposta do servidor.

## Veja Também

- [Documentação oficial da biblioteca Net::HTTP](https://ruby-doc.org/stdlib/libdoc/net/http/rdoc/Net/HTTP.html)
- [Tutorial sobre como enviar uma solicitação HTTP no Ruby](https://www.digitalocean.com/community/tutorials/how-to-use-net-http-with-ruby)
- [Exemplos práticos de uso da biblioteca Net::HTTP](https://www.rubyguides.com/2018/08/net-http-ruby/)