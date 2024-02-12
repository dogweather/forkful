---
title:                "Завантаження веб-сторінки"
aliases:
- /uk/elixir/downloading-a-web-page/
date:                  2024-01-20T17:43:58.763349-07:00
model:                 gpt-4-1106-preview
simple_title:         "Завантаження веб-сторінки"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/downloading-a-web-page.md"
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
