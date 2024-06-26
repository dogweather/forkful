---
date: 2024-01-20 17:43:49.455155-07:00
description: "How to: Elixir, with its powerful HTTP client libraries, makes this\
  \ task a breeze. Here\u2019s how with `HTTPoison`."
lastmod: '2024-03-13T22:44:59.781399-06:00'
model: gpt-4-1106-preview
summary: Elixir, with its powerful HTTP client libraries, makes this task a breeze.
title: Downloading a web page
weight: 42
---

## How to:
Elixir, with its powerful HTTP client libraries, makes this task a breeze. Here’s how with `HTTPoison`:

```elixir
# First, add HTTPoison to your mix.exs dependencies:
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end

# Run mix deps.get to fetch the new dependency

# Now, let's download a web page:
defmodule PageDownloader do
  def download(url) do
    case HTTPoison.get(url) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        {:ok, body}
      {:ok, %HTTPoison.Response{status_code: status_code}} ->
        {:error, "Received status code: #{status_code}"}
      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, reason}
    end
  end
end

# Example usage:
{:ok, contents} = PageDownloader.download("http://example.com")
```

Make sure you handle the potential errors to avoid crashes!

## Deep Dive
Elixir’s approach to web interactions is powered by Erlang's robust networking abilities. `HTTPoison` is a popular library built on top of `hackney`, but it's not the only player. There's also `Tesla`, which offers a more modular approach with middleware support.

Historically, downloading web content was more manual, involving crafting HTTP requests over sockets. Elixir libraries abstract these details, letting you focus on your application logic instead.

When downloading web pages, you deal with asynchronous operations and various HTTP protocols, which Elixir handles gracefully due to its concurrency model and fault-tolerant design. Plus, handling text and binary data is critical—make sure you're considering encoding and the potential for binary data in web content.

## See Also
- [`HTTPoison` documentation](https://hexdocs.pm/httpoison)
- [`Tesla` library on Hex](https://hex.pm/packages/tesla)
- [Elixir School's guide on OTP Concurrency](https://elixirschool.com/en/lessons/advanced/otp-concurrency/)
- [Erlang's `hackney` library](https://github.com/benoitc/hackney)
