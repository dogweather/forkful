---
title:                "Inviare una richiesta http"
date:                  2024-01-20T17:59:30.872356-07:00
model:                 gpt-4-1106-preview
simple_title:         "Inviare una richiesta http"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
Inviare una richiesta HTTP significa chiedere dati o inviare informazioni a un server web. I programmatori lo fanno per interagire con API, servizi web e per scambiare dati tra sistemi.

## How to:
Usiamo `HTTPoison`, una libreria popolare di Elixir per fare richieste HTTP. Installala aggiungendo `{:httpoison, "~> 1.8"}` al tuo `mix.exs` e eseguendo `mix deps.get`.

```elixir
defmodule HttpExample do
  def fetch_content(url) do
    case HTTPoison.get(url) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        IO.puts("Content fetched successfully!")
        IO.puts(body)
      {:ok, %HTTPoison.Response{status_code: code}} ->
        IO.puts("Oops! Something went wrong. Status code: #{code}")
      {:error, %HTTPoison.Error{reason: reason}} ->
        IO.puts("Error fetching content: #{reason}")
    end
  end
end

# Uso:
HttpExample.fetch_content("https://jsonplaceholder.typicode.com/posts/1")
```

Output atteso (esempio):
```
Content fetched successfully!
{
  "userId": 1,
  "id": 1,
  ...
}
```

## Deep Dive
HTTPoison si basa su `hackney`, un client HTTP scritto in Erlang. Un'alternativa a `HTTPoison` è `Tesla`, un altro client HTTP, che permette una maggiore modularità con middleware. Quando invii richieste HTTP, tieni a mente i limiti come il rate limit e il timeout.

## See Also
- [HTTPoison documentation](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Tesla GitHub repository](https://github.com/teamon/tesla)
