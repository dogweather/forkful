---
title:                "Envio de uma solicitação http com autenticação básica"
html_title:           "Elixir: Envio de uma solicitação http com autenticação básica"
simple_title:         "Envio de uma solicitação http com autenticação básica"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por que

Neste artigo, vamos discutir a importância de enviar uma solicitação HTTP com autenticação básica. Especificamente, veremos como isso pode ser útil para acessar recursos protegidos em APIs e outras aplicações que requerem autenticação.

## Como Fazer

Para enviar uma solicitação HTTP com autenticação básica em Elixir, usamos a biblioteca HTTPoison. Primeiro, precisamos adicionar a dependência em nosso `mix.exs`:

```elixir
defp deps do
  [{:httpoison, "~> 1.6"}]
end
```

Em seguida, instale as dependências com o comando `mix deps.get`. Depois disso, podemos utilizar a função `HTTPoison.get/4` para enviar uma solicitação GET com autenticação básica. Por exemplo, se quisermos acessar um recurso protegido em uma API, primeiro precisamos codificar nossas credenciais em Base64:

```elixir
auth_string = Base.encode("username:password")
```

Em seguida, podemos enviar a solicitação utilizando a função `HTTPoison.get/4`:

```elixir
url = "<API_URL>"
headers = [{"Authorization", "Basic " <> auth_string}]
response = HTTPoison.get(url, headers, [], [timeout: 5000])
```

Com isso, nossa solicitação será enviada com as credenciais codificadas e podemos acessar o recurso protegido.

## Profundando o Assunto

A autenticação básica é um método simples de autenticação em que as credenciais são codificadas em Base64 e enviadas com a solicitação HTTP. No entanto, é importante notar que essa forma de autenticação não é considerada segura, pois as credenciais são enviadas em texto simples e podem ser facilmente interceptadas.

Além disso, para acessar recursos protegidos em APIs, muitas vezes é necessário gerar um token de acesso que é enviado em vez das credenciais. Isso é feito para evitar o envio contínuo das credenciais em cada solicitação. Portanto, é sempre importante verificar a documentação da API para determinar o método de autenticação adequado.

## Veja Também

- [HTTPoison documentação](https://hexdocs.pm/httpoison/)
- [API do Elixir - Autenticação Básica](https://hexdocs.pm/elixir/1.12/HTTPoison.BearerAuth.html)
- [Artigo sobre autenticação em APIs](https://nandovieira.com.br/usando-o-httpoison-com-autenticacao-em-apis)