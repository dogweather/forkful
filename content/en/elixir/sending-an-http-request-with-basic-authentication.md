---
title:                "Elixir recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why

Sending an HTTP request with basic authentication is essential for interacting with any web service or API that requires user authentication. By providing the necessary credentials in the request header, you can securely access and retrieve data from the target server.

## How To

To send an HTTP request with basic authentication in Elixir, we first need to import the necessary libraries using the `HTTPoison` and `Base64` modules.

```Elixir
  import HTTPoison
  import Base64
```

Next, we will create a `basic_auth` function that takes in the username and password as parameters and encodes them in Base64 format, as required for basic authentication. We will also specify the necessary headers for the request.

```Elixir
  def basic_auth(username, password) do
    headers = [
      {"Authorization", "Basic #{Base64.encode_to_string("#{username}:#{password}")}"}
    ]
  end
```

Now, we can use the `get` or `post` functions from `HTTPoison` to send our request. We will pass in the URL and headers as parameters, along with any other necessary options.

```Elixir
  get("https://example.com/api/resource", headers, [ssl: [verify: false]])
```

If the request is successful, we will receive a response in the form of a tuple containing the status code and body.

```Elixir
  {:ok, %HTTPoison.Response{status_code: 200, body: "Data returned from server"}}
```

## Deep Dive

One important thing to note about basic authentication is that it is not a secure method of authentication, as the credentials are sent in plain text. Therefore, it is highly recommended to use HTTPS instead of HTTP when using basic authentication.

Additionally, it is important to keep in mind that basic authentication should only be used for simple authentication purposes. For more complex authentication requirements, it is recommended to use other methods such as OAuth or API keys.

## See Also

- [HTTPoison documentation](https://hexdocs.pm/httpoison/)
- [Base64 module documentation](https://hexdocs.pm/elixir/Base64.html)