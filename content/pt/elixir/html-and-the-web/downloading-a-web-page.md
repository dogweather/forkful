---
date: 2024-01-20 17:43:57.594816-07:00
description: "How to: Para baixar uma p\xE1gina web em Elixir, voc\xEA pode usar a\
  \ biblioteca HTTPoison. Primeiro, adicione `httpoison` ao seu `mix.exs`."
lastmod: '2024-03-13T22:44:46.236960-06:00'
model: gpt-4-1106-preview
summary: "Para baixar uma p\xE1gina web em Elixir, voc\xEA pode usar a biblioteca\
  \ HTTPoison."
title: "Baixando uma p\xE1gina da web"
weight: 42
---

## How to:
Para baixar uma página web em Elixir, você pode usar a biblioteca HTTPoison. Primeiro, adicione `httpoison` ao seu `mix.exs`:

```elixir
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end
```

Execute `mix deps.get` para instalar a dependência. Agora vamos baixar uma página:

```elixir
defmodule PageDownloader do
  def download(url) do
    case HTTPoison.get(url) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        {:ok, body}
      {:ok, %HTTPoison.Response{status_code: status_code}} ->
        {:error, "Erro: #{status_code}"}
      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, "Erro: #{inspect(reason)}"}
    end
  end
end

# Exemplo de uso:
PageDownloader.download("http://elixir-lang.org")
```
Saída esperada será o HTML da página, ou um erro caso não consiga baixar.

## Deep Dive
Historicamente, baixar páginas da web em Elixir não era tão direto quanto hoje. Com o lançamento de bibliotecas como HTTPoison e Tesla, a tarefa se simplificou. Alternativamente, a biblioteca `HTTPotion` também pode ser usada, mas `HTTPoison` é mais popular e possui uma API mais amigável.

`HTTPoison` se baseia no `hackney`, um client HTTP em Erlang. Isso permite que você faça requisitos HTTP assíncronos e suporta recursos como SSL, cookies e redirecionamentos.

Além do HTTPoison, para tarefas mais complexas, como navegar pelo JavaScript renderizado ou manter uma sessão, você pode precisar usar ferramentas como o PhantomJS juntamente com Elixir, ou até explorar o uso de headless browsers controlados por Elixir.

## See Also
- [HTTPoison no Hex.pm](https://hex.pm/packages/httpoison)
- [Git Repo HTTPoison](https://github.com/edgurgel/httpoison)
- [HTTPoison Documentation](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Tesla](https://github.com/teamon/tesla) - Uma biblioteca alternativa para cliente HTTP em Elixir.
