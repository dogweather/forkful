---
title:                "Sending an http request with basic authentication"
html_title:           "Elixir recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why

If you're working with web APIs, chances are you'll encounter the need to send an HTTP request with basic authentication. This is a commonly used authentication method that allows you to send sensitive information, such as a username and password, to a server in a secure manner.

## How To

Sending an HTTP request with basic authentication in Elixir is fairly straightforward. First, we need to import the necessary modules: `HTTPoison` for making HTTP requests and `Base64` for encoding our credentials.

```Elixir
import HTTPoison
import Base64
```

Next, we need to construct our request and add the basic authentication header. We can do this using the `request/5` function provided by HTTPoison. In this example, we'll be sending a `GET` request to the GitHub API.

```Elixir
response = HTTPoison.request(
  :get,
  "https://api.github.com/user",
  headers: [
    {"Authorization", "Basic " <> Base64.encode64("username:password")}
  ]
)
```

The authorization header requires our credentials to be Base64 encoded, which is why we used the `Base64.encode64/1` function.

We can then handle the response accordingly, for example by parsing the JSON and retrieving the desired information.

```Elixir
{:ok, %{body: body}} = response
user = Jason.decode!(body)
IO.inspect user["login"]
# outputs "johndoe"
```

## Deep Dive

In basic authentication, the username and password are encoded with Base64 but are still easily decoded by anyone who intercepts the request. This means it should only be used over HTTPS connections to ensure the security of the credentials.

Another important aspect to keep in mind is that basic authentication is considered a weak form of authentication. It is recommended to use more secure methods such as OAuth or token-based authentication for better protection against potential security threats.

## See Also

- [HTTPoison](https://github.com/edgurgel/httpoison) - Elixir HTTP client library
- [Base64](https://hexdocs.pm/elixir/Base64.html) - Elixir module for encoding and decoding Base64 data