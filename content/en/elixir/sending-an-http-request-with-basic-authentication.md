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

## What & Why?

Sending an HTTP request with basic authentication is a way for programmers to securely access web services that require authentication. This method allows the client to include a username and password with each request, providing the necessary credentials to access the protected resource. It is commonly used for accessing APIs and web services that require authentication for security purposes.

## How to:

To send an HTTP request with basic authentication in Elixir, you can use the built-in [HTTPoison](https://github.com/edgurgel/httpoison) library. First, you will need to provide the username and password in the request options using the `auth` option. Here’s an example of sending an HTTP GET request to an API endpoint with basic authentication:

```Elixir
url = "https://example.com/api/resource"
auth_options = [auth: {"username", "password"}]
response = HTTPoison.get(url, [], auth_options)
```

The response object will contain the status code, response headers, and body of the API response. You can also specify a `timeout` option to set the maximum time to wait for a response.

## Deep Dive:

Sending HTTP requests with basic authentication has been a widely used security measure to protect web services and APIs. It was first introduced in 1996 as part of the HTTP/1.0 standard and has remained a popular authentication method. Alternatives to basic authentication include OAuth, bearer tokens, and more advanced methods such as public key authentication.

In Elixir, the `auth` option in the HTTPoison library automates the process of adding the `Authorization` header with the encoded username and password to the HTTP request. This makes it easy for programmers to implement basic authentication in their code without having to manually add headers or encode the credentials.

## See Also:

- [HTTPoison Documentation](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Basic Authentication Overview](https://www.jw32.org/2005/http-authentication/)
- [HTTP/1.0 Specification](https://tools.ietf.org/html/rfc1945)