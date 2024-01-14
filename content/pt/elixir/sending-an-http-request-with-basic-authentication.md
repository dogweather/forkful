---
title:                "Elixir: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Porquê

Por que alguém se envolveria em enviar uma solicitação HTTP com autenticação básica? Bem, a autenticação básica é uma forma comum de autenticação usada em muitas aplicações da web. Isso permite que usuários ou sistemas se autentiquem com um nome de usuário e uma senha simples antes de acessar determinados recursos ou informações.

## Como Fazer

Agora que sabemos por que usar a autenticação básica, vamos dar uma olhada em como fazê-lo usando Elixir. Primeiro, vamos importar o módulo `HTTPoison` para lidar com a criação e envio de solicitações HTTP.

```
Elixir
defmodule HTTPRequest do
  require HTTPoison

  #Enviar uma solicitação GET com autenticação básica
  def send_request(url, username, password) do
    response = HTTPoison.get(url, [], [basic_auth: {username, password}])
  end
end
```

Aqui, criamos um módulo chamado `HTTPRequest` e definimos uma função `send_request` que recebe uma URL, um nome de usuário e uma senha como parâmetros. Usando o `HTTPoison`, podemos enviar uma solicitação GET para a URL especificada e configurar a autenticação básica com as credenciais fornecidas. O resultado é armazenado na variável`response` para que possamos usá-la posteriormente.

Agora, vamos testar nossa função chamando-a com algumas credenciais de exemplo:

```
Elixir
response = HTTPRequest.send_request("https://exemplo.com", "usuario", "senha")
IO.inspect response
```

Isso deve retornar informações sobre a resposta, incluindo o status, o corpo e os cabeçalhos. Você pode personalizar o código para lidar com diferentes tipos de solicitações e respostas, dependendo das necessidades da sua aplicação.

## Mergulho Profundo

Agora que vimos como enviar uma solicitação com autenticação básica usando Elixir, vamos explorar mais detalhadamente o processo de autenticação básica. Ao enviar uma solicitação com a autenticação básica, as credenciais do usuário são codificadas em Base64 e incluídas no cabeçalho `Authorization`. O formato é o seguinte:

```
Authorization: Basic <string de credenciais em Base64>
```

Para codificar as credenciais em Base64, você pode usar a função `encode` do módulo `Base` ou a função `encode64` do módulo `:base64`. Certifique-se de que suas credenciais sejam fornecidas como `<usuário>:<senha>` antes de codificá-las.

Além disso, é importante lembrar que a autenticação básica é um método de autenticação inseguro, pois as credenciais são enviadas sem criptografia. Portanto, é recomendável usá-lo apenas para solicitações que não envolvam informações sensíveis.

## Veja Também

- [Documentação HTTPoison](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Base - Elixir](https://hexdocs.pm/elixir/Base.html)
- [:base64 - Erlang](http://erlang.org/doc/man/base64.html)