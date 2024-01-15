---
title:                "Sending an http request"
html_title:           "Elixir recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why

Sending HTTP requests is a common task in web development. It allows us to communicate with other web servers, retrieve data, and integrate our applications with external services.

## How To

Sending an HTTP request in Elixir is straightforward. First, we need to use the `HTTPoison` library, which provides a simple and easy-to-use interface for making HTTP requests.

```Elixir
# Import HTTPoison
import HTTPoison

# Make a GET request to a URL
response = HTTPoison.get("http://example.com")

# Display the response status code
IO.inspect response.status_code 

# Display the response body
IO.inspect response.body
```
Sample Output:
```
200
<!DOCTYPE html>
<html>
<head>
<title>Example Domain</title>
...
</html>
```

The first line imports the `HTTPoison` module into our code. Then, we use the `get/1` function to make a GET request to the specified URL. The `get/1` function takes the URL as a parameter and returns a `HTTPoison` response. We can access the status code and body of the response using the `status_code` and `body` fields, respectively.

We can also send other types of HTTP requests, such as POST and PUT, by using the `post/2` and `put/2` functions and passing in the request body as the second parameter.

```Elixir
# Make a POST request with a JSON body
response = HTTPoison.post("http://example.com/post", %{name: "John", age: 30})

# Make a PUT request with a form body
response = HTTPoison.put("http://example.com/user", {:form, "name=John&age=30"})
```

## Deep Dive

The `HTTPoison` library is built on top of the `hackney` HTTP client, making it a reliable and performant choice for making HTTP requests in Elixir. It also supports different adapters, such as `:hackney` and `:ibrowse`, allowing us to choose the one that best fits our application's needs. Additionally, `HTTPoison` handles redirects, authentication, and other common HTTP features, making it a convenient option for handling HTTP requests.

See Also
- [HTTPoison Documentation](https://hexdocs.pm/httpoison/)
- [Elixir HTTP Clients](https://github.com/h4cc/awesome-elixir#http-clients)