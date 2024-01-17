---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Elixir: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O que & Por que?

Enviar uma solicitação HTTP com autenticação básica é o ato de incluir um cabeçalho de autorização em uma solicitação HTTP para acessar um determinado recurso protegido. Os programadores geralmente fazem isso para acessar APIs ou páginas da web que requerem autenticação.

## Como fazer:

```Elixir
  httpc.request(:get, "https://meusite.com/recurso", [
    {:basic_auth, {"usuario", "senha"}}
  ])
```

Saída de exemplo:

```
{:ok, %HTTPoison.Response{
  status_code: 200,
  body: "Dados do recurso protegido"
}}
```

## Mergulho Profundo:

Este método de autenticação foi introduzido no HTTP 1.0 como uma forma de autenticação básica através do uso de um cabeçalho de autorização. Existem outras opções de autenticação, como a autenticação digest, que é mais segura, mas também mais complexa de implementar.

Ao enviar uma solicitação com autenticação básica, a informação de usuário e senha é base64-codificada e incluída no cabeçalho de autorização. Isso torna a comunicação menos segura, pois é fácil de decodificar e potencialmente expõe as credenciais do usuário.

## Veja também:

[Documentação oficial do Elixir sobre o módulo HTTPc](https://hexdocs.pm/elixir/HTTPc.html)

[Artigo sobre autenticação HTTP básica na prática](https://www.apollographql.com/blog/authenticating-with-elixir/)