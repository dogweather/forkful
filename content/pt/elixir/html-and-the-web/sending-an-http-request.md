---
date: 2024-01-20 17:59:19.422312-07:00
description: "Enviar uma requisi\xE7\xE3o HTTP significa pedir informa\xE7\xE3o ou\
  \ enviar dados para um servidor web usando o protocolo HTTP. Programadores fazem\
  \ isso para\u2026"
lastmod: '2024-03-13T22:44:46.235151-06:00'
model: gpt-4-1106-preview
summary: "Enviar uma requisi\xE7\xE3o HTTP significa pedir informa\xE7\xE3o ou enviar\
  \ dados para um servidor web usando o protocolo HTTP. Programadores fazem isso para\u2026"
title: "Enviando uma requisi\xE7\xE3o HTTP"
---

{{< edit_this_page >}}

## O Que é & Porquê?
Enviar uma requisição HTTP significa pedir informação ou enviar dados para um servidor web usando o protocolo HTTP. Programadores fazem isso para interagir com APIs web, acessar serviços remotos e trocar informações com outras aplicações.

## Como Fazer:
Para enviar requisições HTTP em Elixir, você pode usar a biblioteca HTTPoison. Primeiro, adicione ao seu `mix.exs`:

```elixir
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end
```

Depois, execute `mix deps.get` para baixar a biblioteca. Agora, você pode fazer uma requisição GET simples com:

```elixir
HTTPoison.get("https://jsonplaceholder.typicode.com/posts/1")
```

E o exemplo de resposta seria algo assim:

```elixir
{:ok, %HTTPoison.Response{
  status_code: 200,
  body: "{...json body...}",
  headers: [{...headers...}]
}}
```

## Aprofundando:
Historicamente, em Elixir, a biblioteca padrão para fazer requisições HTTP era a `HTTPotion`, mas com o tempo, a `HTTPoison` se tornou mais popular por sua simplicidade e uma API mais amigável. Utilizando o adapter `hackney`, `HTTPoison` fornece um conjunto de funções para fazer requisições de forma síncrona e assíncrona. Existem alternativas, como a `Tesla`, que é mais extensível com middleware customizável, ou a recém chagada `Mint`, que é uma biblioteca de baixo nível que oferece um controle mais granular sobre as conexões HTTP. Na implementação, ao enviar uma requisição, você geralmente lidará com respostas e erros, parseamento de JSON e tratamento de timeouts e retries.

## Veja Também:
- Documentação oficial do HTTPoison: https://hexdocs.pm/httpoison/
- Repositório da biblioteca Tesla: https://github.com/teamon/tesla
- Documentação da biblioteca Mint: https://hexdocs.pm/mint/
- JSONPlaceholder para testar requisições: https://jsonplaceholder.typicode.com/
