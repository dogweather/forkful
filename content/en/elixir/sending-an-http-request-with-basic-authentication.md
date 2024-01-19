---
title:                "Sending an http request with basic authentication"
html_title:           "Fish Shell recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Send an HTTP Request with Basic Authentication in Elixir

## What & Why?

Sending an HTTP request with basic authentication in Elixir means setting up a secure communication channel between the client and the server over HTTP. Programmers do this to protect sensitive information when the client interacts with the server.

## How to:

In Elixir, we use the [HTTPoison library](https://hexdocs.pm/httpoison/readme.html) to send HTTP requests. First, install it by adding `httpoison` to your list of dependencies in `mix.exs`:

```Elixir
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end
```

Now, let's send an HTTP request with Basic Authentication:

```Elixir
defmodule MyModule do
  def send_request do
    case HTTPoison.get("https://api.example.com", ["Authorization": basic_authorization_header()]) do
      {:ok, response} -> IO.inspect(response.status_code)
      {:error, reason} -> IO.inspect(reason)
    end
  end

  defp basic_authorization_header do
    credentials = "#{System.get_env("API_USER")}:#{System.get_env("API_PASSWORD")}"
    "Basic " <> Base.encode64(credentials)
  end
end
```

When we run `send_request/0` function, it makes a GET request to the target URL and outputs the status code of the response.

## Deep Dive

The basic authentication in HTTP is a method designed to allow a web browser, or other client program, to provide credentials in the form of a username and password pair. Its notoriety for its simplicity is matched by its criticisms for its lack of protection against eavesdropping or 'man-in-the-middle' attacks. 

Alternatives include digest authentication or token-based systems like OAuth, which add an extra layer of security by minimizing raw credential exposure. 

In Elixir, when we make the request with HTTPoison, the `:ssl` and `:httpc` options in the request take an important role in setting up the secure connection. While the request is sent, the `Authorization` header, encoded in Base64, is included which carries the credentials.

## See Also

- [Official Elixir documentation](https://elixir-lang.org/docs.html)
- [HTTPoison documentation](https://hexdocs.pm/httpoison/readme.html)
- [Phoenix framework for Elixir](https://hexdocs.pm/phoenix/overview.html)
- [OWASP guide to authentication](https://owasp.org/www-project-cheat-sheets/cheatsheets/Authentication_Cheat_Sheet.html)