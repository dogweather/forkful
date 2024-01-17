---
title:                "Sending an http request with basic authentication"
html_title:           "Gleam recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Welcome to the World of Basic Authentication in HTTP Requests!

## What & Why?
Sending an HTTP request with basic authentication simply means adding a username and password to your request in order to access protected resources on a server. Programmers use this method to ensure secure communication with a server, as it allows only authorized users to retrieve the requested information.

## How to:
Let's dive into some coding examples to demonstrate how to send an HTTP request with basic authentication in Gleam:

```Gleam
import gleam/http

// Create an HTTP client with a basic authentication config
client = http.make_client(~config=http.BasicConfig(username="username", password="password"))

// Make a GET request to an endpoint with basic authentication
response = http.get(client, "https://example.com/protected-resource")

// Print the response status code
log(response.status_code)

// Print the response body
log(response.body)
```

Sample output:
```
200
"This is a protected resource for authorized users only."
```

## Deep Dive:
In the early days of the internet, basic authentication was the primary method for securing access to restricted content on servers. However, with the rise of more advanced authentication methods such as OAuth and API keys, basic authentication is now considered less secure. It also has limitations, such as being unable to revoke access for individual users without changing the shared password.

In Gleam, basic authentication is implemented through the use of the `http.BasicConfig` type, which contains the username and password needed for authentication. This configuration is then passed into the `http.make_client` function, which creates an HTTP client that can be used to make requests with the specified authentication.

## See Also:
If you want to learn more about sending HTTP requests in Gleam, check out the official Gleam documentation. You can also explore other authentication methods such as OAuth or API keys for a more secure and flexible way of accessing restricted resources. Happy coding!