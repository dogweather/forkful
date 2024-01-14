---
title:                "Elixir recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why
As programmers, we often come across tasks that require us to interact with web pages. Whether it's for scraping data, testing web applications, or simply automating repetitive tasks, downloading a web page is a common requirement. In this blog post, we will explore how to download a web page using Elixir.

## How To
To start off, we will need to install the HTTPoison library, which allows us to make HTTP requests in Elixir. We can do this by adding `{:httpoison, "~> 1.5"}` to our `mix.exs` file and running `mix deps.get` in the terminal.

Next, let's set up our Elixir project by creating a file named `download.exs` and adding the following code:

```Elixir
defmodule Downloader do
  @base_url "https://www.example.com"

  def download_page(path) do
    url = @base_url <> path
    case HTTPoison.get(url) do
      {:ok, %HTTPoison.Response{body: body}} ->
        IO.puts("Successfully downloaded #{path}")
        IO.inspect(body)
      {:error, %HTTPoison.Error{reason: reason}} ->
        IO.puts("Error downloading #{path}: #{reason}")
    end
  end
end

Downloader.download_page("/home")
```

In the above code, we defined a module named `Downloader` and a function `download_page` that takes in a path as a parameter. We then construct the full URL by appending the path to our base URL and use `HTTPoison.get` to make a GET request. Depending on the response, we either print the body of the page or an error message.

To run our code, we can use `elixir download.exs` in the terminal. We should see the body of the page printed out if the request was successful.

## Deep Dive
Let's take a closer look at the `HTTPoison.get` function. This function returns a tuple with either `:ok` or `:error` as the first element. If it is `:ok`, the second element of the tuple will be an `%HTTPoison.Response` struct, which contains information about the response, including the body.

We can also pass in additional headers and options to our request, such as setting a timeout or adding authentication. Refer to the HTTPoison documentation for more details.

Furthermore, instead of using `IO.puts` and `IO.inspect` to print the response, we can use the `HTTPoison.Response.body_to_string` function to convert the response body to a string for further processing.

## See Also
- [HTTPoison documentation](https://hexdocs.pm/httpoison/1.5.1/HTTPoison.html)
- [Elixir School - HTTP requests with HTTPoison](https://elixirschool.com/en/lessons/libraries/http/)
- [Elixir Forum - HTTPoison discussion thread](https://elixirforum.com/t/when-to-use-httpoison-and-when-to-use-hackney/16216)