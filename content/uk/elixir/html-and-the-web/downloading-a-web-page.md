---
date: 2024-01-20 17:43:58.763349-07:00
description: "Downloading a web page means grabbing its contents from the Internet:\
  \ HTML, CSS, JavaScript, images, etc. Programmers do it to interact with web data,\u2026"
lastmod: '2024-03-11T00:14:22.549087-06:00'
model: gpt-4-1106-preview
summary: "Downloading a web page means grabbing its contents from the Internet: HTML,\
  \ CSS, JavaScript, images, etc. Programmers do it to interact with web data,\u2026"
title: "\u0417\u0430\u0432\u0430\u043D\u0442\u0430\u0436\u0435\u043D\u043D\u044F \u0432\
  \u0435\u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043A\u0438"
---

{{< edit_this_page >}}

## Що це таке і для чого?

Downloading a web page means grabbing its contents from the Internet: HTML, CSS, JavaScript, images, etc. Programmers do it to interact with web data, automate tasks, test apps or scrape information. 

## Як це зробити:

Elixir makes it easy with libraries like HTTPoison or Mint. Here's a quick example using HTTPoison.

```elixir
# Add HTTPoison to your mix.exs dependencies
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end

# Run mix deps.get to install the dependency

# Now, you can use HTTPoison to download a web page
defmodule WebPageDownloader do
  def get_page_content(url) do
    case HTTPoison.get(url) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        body
      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, reason}
    end
  end
end

# Sample usage
IO.puts WebPageDownloader.get_page_content("https://example.com")
```

Sample output could be the HTML content of "https://example.com".

## Поглиблений розгляд:

Initially, Elixir relied on :httpc module from Erlang. However, the community desired improved usability & performance, leading to libraries like HTTPoison, based on hackney, and Mint, a newer, low-level HTTP client.

Alternatives to HTTPoison include Mint for more control and Tesla for a flexible middleware-based approach. Mint provides a neat interface for concurrent stream handling but requires more manual work, while Tesla lets you swap HTTP clients and add features like logging.

When downloading a webpage, handling redirects, SSL, and compressed content are considerations. Libraries address these. For example, HTTPoison auto-follows redirects and handles SSL.

## Дивіться також:

- HTTPoison documentation: https://hexdocs.pm/httpoison
- Mint GitHub repo: https://github.com/elixir-mint/mint
- Tesla documentation: https://hexdocs.pm/tesla
- Elixir Forum for discussions: https://elixirforum.com
