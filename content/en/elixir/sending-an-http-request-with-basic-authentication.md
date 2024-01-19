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

# Hitting Home with HTTP in Elixir: Basic Authentication Made Simple 

## What & Why?
The act of sending an HTTP request with basic authentication in Elixir involves making an API call and including a username and password in the header. This is common practice when your code needs to interact with a protected resource.

## How to:
The most straightforward way to make authenticated HTTP requests in Elixir is to use the popular `HTTPoison` library. With `HTTPoison`, requests become impressively clean and simple. Here’s how to get it done:

```Elixir
{:ok, response} = HTTPoison.get("https://reqres.in/api/users/2", ["Authorization": "Basic #{:base64.encode_to_string('user:password')}"])
IO.inspect response.body
```
In the above code, you're telling HTTPoison to send a GET request to the provided URL with an Authorization header containing the base64 encoded username and password.

When you run this code, you should see the server's response printed to your console.

## Deep Dive
In the earlier days of Elixir, developers had to fall back to Erlang libraries to make HTTP requests, and not all of them were user-friendly or thoroughly documented. `HTTPoison`, an Elixir wrapper around the `hackney` Erlang library, simplified matters considerably.

It's worth noting that Elixir's standard library now includes a module called `:httpc`, which also supports basic authentication. Still, `HTTPoison` is often the more intuitive choice.

If for some reason you need to create the 'Authorization' header by hand, be aware that the string 'user:password' must be encoded in base64. The final result should adhere to this format: "Authorization: Basic {base64_encoded_string}"

## See Also
For more information on using `HTTPoison` and `Elixir` for HTTP requests, check out the following sources:

- `HTTPoison` GitHub Repo: https://github.com/edgurgel/httpoison
- `hackney` GitHub Repo: https://github.com/benoitc/hackney
- Elixir's `:httpc` module: https://erlang.org/doc/man/httpc.html
- Basic HTTP Authentication: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication