---
title:                "Downloading a web page"
html_title:           "Elixir recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a web page means retrieving the HTML code and resources of a website from a remote server to display it on a local device. Programmers do this to access website content, scrape data, or automate tasks such as filling out web forms.

## How to:

Using Elixir, downloading a web page can be done in just a few lines of code. First, we need to install the `HTTPoison` dependency. Then, we can use the `get` function to specify the URL of the webpage we want to download.

```
Elixir
def deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end
```

```
Elixir
url = "https://www.example.com"
response = HTTPoison.get(url)
```

The `response` variable will now contain the HTML code of the webpage as well as information about the response, such as status code and headers. We can access the HTML code by using `response.body` in our code.

## Deep Dive:

Downloading web pages has long been an essential task for programmers, especially for those involved in web scraping or automation. Before Elixir, it was primarily done using languages such as Python, JavaScript, or Ruby.

In Elixir, besides `HTTPoison`, there are other libraries available for downloading web pages, such as `Mint`, `Finch`, and `Crawly`. Each offers different features, and it's up to the programmer to choose the most suitable one for their project.

Behind the scenes, Elixir uses the Erlang-based `httpc` client to handle the requests. This client supports features such as redirects, authentication, and HTTPS.

## See Also:

- Documentation for `HTTPoison`: https://hexdocs.pm/httpoison/1.0.3/HTTPoison.html
- Comparison of web scraping libraries in Elixir: https://akoutmos.com/post/comparing-web-scraping-libraries-in-elixir/