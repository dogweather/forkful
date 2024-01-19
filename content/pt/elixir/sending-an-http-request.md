---
title:                "Enviando uma solicitação http"
html_title:           "Bash: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Enviar uma solicitação HTTP é como o seu programa comunica com outros sistemas para buscar ou enviar dados. Programadores usam isso para integrar seu software com serviços web, APIs, ou para interagir com outros programas remotamente.

## Como Fazer:

Aqui está um exemplo de como enviar uma requisição GET usando a biblioteca HTTPoison no Elixir.

```elixir
HTTPoison.start

{:ok, response} = HTTPoison.get("http://example.com")
IO.puts response.body
```
Quando executa este código, ele pegará os dados do `http://example.com`, e imprimirá o corpo da resposta no console.

E aqui está um exemplo de um POST request, onde estamos enviando dados JSON para o servidor:

```elixir
headers = ["Content-Type": "application/json"]
body = Poison.encode!(%{"chave": "valor"})

{:ok, response} = HTTPoison.post("http://example.com", body, headers)
IO.puts response.body
```

## Aprofundamento:

No contexto histórico, o protocolo HTTP foi criado para permitir a comunicação entre servidores web e clientes. Mais tarde, ele começou a ser amplamente utilizado para permitir a comunicação entre programas.

Existem muitas alternativas para enviar solicitações HTTP em Elixir, incluindo o HTTPoison mencionado, Tesla e o built-in Erlang httpc. Cada um destes tem suas próprias vantagens e desvantagens dependendo das necessidades específicas do seu projeto.

A mágica por trás de enviar uma solicitação HTTP está na implementação do protocolo TCP/IP que está embutida no sistema operacional. Elixir, em seguida, usa a biblioteca Erlang :httpc para fazer chamadas HTTP na maior parte do tempo.

## Ver Também:

- HTTPoison no Hex: https://hex.pm/packages/httpoison
- Tesla no Hex: https://hex.pm/packages/tesla
- Documentação oficial da biblioteca Erlang :httpc: http://erlang.org/doc/man/httpc.html