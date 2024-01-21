---
title:                "Sending an http request with basic authentication"
date:                  2024-01-20T18:01:29.109075-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sending an http request with basic authentication"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request with basic authentication involves adding a username and password to your request to access a protected resource. Programmers do it to ensure secure data access and transfer, keeping unauthorized users out.

## How to:

To send an HTTP request with basic authentication in Elixir, you can use the `HTTPoison` library:

```elixir
# Add HTTPoison to your project's dependencies in mix.exs
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end

# Now let's make a request with basic auth

# Run `mix deps.get` to fetch the dependency

# In your Elixir code
defmodule BasicAuthRequest do
  def send_request do
    # Encode credentials "username:password" using Base64
    basic_auth =
      "username:password"
      |> Base.encode64()

    # Set the Authorization header
    headers = [{"Authorization", "Basic #{basic_auth}"}]

    # Make a GET request to https://example.com/protected-resource
    HTTPoison.get("https://example.com/protected-resource", headers)
  end
end

# Call the function
{:ok, response} = BasicAuthRequest.send_request()
IO.inspect(response.status_code)  # Should be 200 if authentication is successful
```

If basic authentication is successful, you'll get a `200` status code. Failed authentication typically results in a `401`.

## Deep Dive

Basic authentication is a part of HTTP defined in RFC 7617, dating back to the very early web. It's simple but less secure than modern methods, sending credentials in every request (base64 encoded not encrypted).

Alternatives include:
- OAuth: An open standard for access delegation, used to grant websites or applications access to their information on other websites without giving them the passwords.
- API Keys: Unique identifiers used to authenticate a user or a program in API requests.
- Bearer Tokens/JWT: Security tokens that contain all the user data necessary to authenticate the user.

Implementation-wise, when using `HTTPoison`, we:
- Base64 encode the username and password;
- Include this encoded string in the `Authorization` header prefixed by "Basic ";
- Send the request to the server hoping access is granted.

Remember, basic auth is clear-text and can be easily decoded. It's safe only over HTTPS.

## See Also

- HTTPoison documentation: https://hexdocs.pm/httpoison
- Basic authentication schema (RFC 7617): https://tools.ietf.org/html/rfc7617
- Elixir's `Base` module documentation: https://hexdocs.pm/elixir/Base.html
- OAuth 2.0 Authorization Framework: https://oauth.net/2/