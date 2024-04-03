---
date: 2024-01-20 17:59:30.340295-07:00
description: "Sending an HTTP request is how your program asks for data from the web,\
  \ sorta like how you'd ask a librarian for a book. Programmers do this to fetch,\u2026"
lastmod: '2024-03-13T22:44:59.779740-06:00'
model: gpt-4-1106-preview
summary: Sending an HTTP request is how your program asks for data from the web, sorta
  like how you'd ask a librarian for a book.
title: Sending an HTTP request
weight: 44
---

## What & Why?
Sending an HTTP request is how your program asks for data from the web, sorta like how you'd ask a librarian for a book. Programmers do this to fetch, send, or manipulate remote data, from getting the weather to posting tweets.

## How to:
Use Elixir's `HTTPoison` library. It's neat, simple, and gets the job done.

1. Add HTTPoison to your project's `mix.exs`:

```elixir
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end
```

2. Run `mix deps.get` in your terminal to fetch the dependency.

3. Now you're set to send a GET request:

```elixir
case HTTPoison.get("https://jsonplaceholder.typicode.com/posts/1") do
  {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
    IO.inspect(body) # you've got your data!
  {:error, %HTTPoison.Error{reason: reason}} ->
    IO.inspect(reason) # handle error
end
```

Sample output: a JSON string of post data from the placeholder API.

## Deep Dive
Historically, you'd use `:httpc` that comes with Erlang/OTP or Elixir's `HTTPotion`. HTTPoison is more popular now, with cleaner syntax and built upon Hackney, a robust HTTP client for Erlang.

Alternatives to HTTPoison include Tesla – a flexible HTTP client with middleware support, and Mint – a shiny, low-level HTTP client.

Implementation wise, these libraries handle connection pooling, SSL, and keep-alive, tricky stuff that's essential for efficient HTTP requests. They act like friendly librarians who handle the nitty-gritty, so you don't have to crawl through the stacks yourself.

## See Also
- [HTTPoison GitHub](https://github.com/edgurgel/httpoison) – for all the details and updates.
- [HexDocs for HTTPoison](https://hexdocs.pm/httpoison) – the place for comprehensive documentation.
- [Elixir Forum](https://elixirforum.com) – to chat with the community.
