---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Clojure: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Enviando uma solicitação HTTP com autenticação básica em Elixir

## O que & Por quê?

Enviar uma solicitação HTTP com autenticação básica é uma maneira de interagir com APIs protegidas que requerem credenciais (nome de usuário e senha) no cabeçalho da solicitação HTTP. Programamos isso para acessar ou manipular dados em tais APIs de forma segura.

## Como fazer:

Elixir, com sua biblioteca `HTTPoison`, nos ajuda a fazer isso. Primeiro, adicione `HTTPoison` ao seu mix.exs:

```Elixir
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end
``` 

Depois, faça a solicitação HTTP com autenticação básica da seguinte forma:

```Elixir
defmodule MyHttpClient do
  def get(url, username, password) do
    headers = ["Authorization": "Basic " <> :base64.encode_to_string("#{username}:#{password}")]
    HTTPoison.get(url, headers)
  end
end
```

O resultado poderia ser algo assim:

```Elixir
{:ok, %HTTPoison.Response{
       body: "<html>...</html>",
       headers: [{"Content-Type", "text/html"}],
       status_code: 200
}}
```

## Mergulho profundo

A autenticação básica é uma das formas mais antigas de autenticação na web, desde os primeiros dias do protocolo HTTP. Há outras alternativas para isso, como o OAuth e o token JWT, que são mais seguros, mas requerem mais complexidade para implementar.

Em Elixir, estamos usando a biblioteca `HTTPoison`, que usa a biblioteca `hackney` Erlang por baixo dos panos. `hackney` é uma das bibliotecas HTTP mais populares em Erlang, e também oferece suporte a autenticação básica. Certifique-se de tratar os erros e as respostas de falha do servidor ao usar essas bibliotecas.

## Veja também

A documentação oficial de Elixir e HTTPoison são boas fontes para aprender mais sobre esse tópico:

- Elixir: [https://elixir-lang.org/docs.html](https://elixir-lang.org/docs.html)
- HTTPoison: [https://hexdocs.pm/httpoison/HTTPoison.html](https://hexdocs.pm/httpoison/HTTPoison.html) 

Hackney também possui uma documentação abrangente:

- Hackney: [https://hexdocs.pm/hackney/readme.html](https://hexdocs.pm/hackney/readme.html)