---
title:                "Sending an http request"
html_title:           "Elixir recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request is a way programs talk with each other across the web. This communication is crucial—it's how you fetch data from servers and APIs.

## How to:
First, get the HTTPoison library. Add this to your mix.exs deps:

```elixir
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end
```

Then pull it in with `mix deps.get`.

Here's a straightforward GET request:

```elixir
defmodule MyModule do
  def perform_request do
    case HTTPoison.get("https://jsonplaceholder.typicode.com/posts") do
      {:ok, response} -> IO.inspect(response)
      {:error, reason} -> IO.inspect(reason)
    end
  end
end
```

Run it, and you'll see the server response output in your terminal.

## Deep Dive

Elixir is built on Erlang, which was made for telecommunication systems. So HTTP requests (transmitting messages) are in the language's DNA.

`HTTPoison.get()` is good, but there's also `HTTPoison.post()`, `HTTPoison.put()`, and `HTTPoison.delete()`. You can add headers, request options, and also work with async requests.

Though HTTPoison is popular, other options you could use in Elixir include Tesla and Hackney.

## See Also
Check out these resources to explore further:

- HTTPoison Github Repo: https://github.com/edgurgel/httpoison
- Official Elixir Documentation: https://elixir-lang.org/docs.html
- Tesla Repo: https://github.com/teamon/tesla
- Hackney Repo: https://github.com/benoitc/hackney