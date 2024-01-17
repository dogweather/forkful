---
title:                "Enviando uma solicitação http"
html_title:           "Elixir: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O que e Por que?

Enviar uma solicitação HTTP é o processo de um programa solicitando informações de um servidor da web. Os programadores fazem isso para obter dados ou executar uma ação em um determinado servidor. 

## Como:

````Elixir

# Usando a biblioteca HTTPoison para enviar uma solicitação GET
response = HTTPoison.get("https://www.exemplo.com")
# O resultado deve ser um objeto de resposta com informações sobre os dados solicitados

# Usando a biblioteca HTTP para enviar uma solicitação POST com um corpo de solicitação
response = HTTP.post("https://www.exemplo.com", body: %{username: "user123", password: "12345"})
# O resultado deve ser um objeto de resposta com informações sobre o sucesso ou falha da solicitação

````

## Mergulho Profundo:

Enviar uma solicitação HTTP é uma prática comum na programação e é essencial para o funcionamento de muitas aplicações da web. Existem diferentes bibliotecas e métodos disponíveis para enviar solicitações HTTP em Elixir, sendo algumas das mais populares o HTTPoison e o HTTP. Além disso, é importante ter cuidado com a segurança ao enviar solicitações HTTP, pois elas podem ser interceptadas por terceiros.

## Veja também:

- [Documentação HTTPoison](https://hexdocs.pm/httpoison/api-reference.html)
- [Documentação HTTP](https://hexdocs.pm/http/api-reference.html)
- [Tutorial sobre enviar solicitações HTTP em Elixir](https://dev.to/brpaz/sending-http-requests-with-elixir-5d9f)