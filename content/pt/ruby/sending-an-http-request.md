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

# O que é e por que fazer:

Enviar uma solicitação HTTP (Hypertext Transfer Protocol) é essencialmente enviar uma mensagem para um servidor de hospedagem web. Os programadores fazem isso para obter informações ou executar ações em um site ou aplicativo, como acessar dados, enviar formulários ou receber respostas de APIs.

# Como fazer:

```ruby
require 'net/http' # importa a biblioteca para enviar solicitações HTTP
uri = URI('https://www.example.com') # define a URI (Uniform Resource Identifier) do site ou API que você deseja acessar
response = Net::HTTP.get_response(uri) # envia a solicitação e espera pela resposta do servidor
puts response.body # exibe o conteúdo da resposta do servidor
```

Output:

```
<!DOCTYPE><html>
<head>
    <title>Example Domain</title> <!-- resposta do servidor -->
    <meta charset="utf-8" />
    <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <style type="text/css">
    /* ... */
    </style> 
</head>

<!--
    Solicitação HTTP pode ser usada para acessar qualquer tipo de conteúdo em um site ou aplicativo, como HTML, JSON e imagens, 
    além de poder executar ações específicas, como fazer POST em um formulário ou autenticar um usuário.
-->

# Profundando:

1. Contexto histórico:
O HTTP foi proposto em 1989 por Tim Berners-Lee, o pai da World Wide Web. É um protocolo universal e independente de plataforma para comunicação entre servidores e clientes, facilitando a troca de dados na internet.

2. Alternativas:
Além do Net::HTTP, que vem incluído na biblioteca padrão do Ruby, existem outras bibliotecas, como HTTParty e Faraday, que oferecem recursos mais avançados para enviar solicitações e receber respostas HTTP.

3. Detalhes de implementação:
A biblioteca Net::HTTP é construída em torno de uma classe homônima, que possui métodos para configurar a solicitação, adicionar cabeçalhos, autenticar com HTTP Basic ou Digest, entre outros recursos. Além disso, ela também usa um objeto Net::HTTPResponse para armazenar a resposta do servidor.

# Veja também:

- [Ruby documentação sobre Net::HTTP](https://ruby-doc.org/stdlib-2.6.3/libdoc/net/http/rdoc/Net/HTTP.html)
- [HTTParty](https://github.com/jnunemaker/httparty)
- [Faraday](https://github.com/lostisland/faraday)