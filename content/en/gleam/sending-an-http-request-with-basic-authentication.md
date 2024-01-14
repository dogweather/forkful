---
title:                "Gleam recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why

Sending HTTP requests is a fundamental part of web programming. In order to access certain resources or perform actions on a web server, you need to send an HTTP request. Basic authentication is a common way of securing these requests and ensuring that only authorized users have access.

## How To

To send an HTTP request with basic authentication in Gleam, you will need to use the `http` and `auth` modules from the standard library. First, we will define our request URL and authentication credentials:

```Gleam
let url = "https://example.com/api"
let auth = http.basic_auth("username", "password")
```

Next, we can use the `http` module to create our request by specifying the HTTP method, URL, and authentication credentials:

```Gleam
let request = http.request("GET", url, [auth])
```

We can then use the `http.send` function to actually send the request and get a response back:

```Gleam
let response = http.send(request)
```

Finally, we can access the response body and status code to see if our request was successful:

```Gleam
let body = response.body
let status = response.status
```

## Deep Dive

Under the hood, basic authentication works by adding a special header, called `Authorization`, to the HTTP request. This header contains the username and password encoded in a specific format. The server then checks this header and verifies the credentials before allowing the request to proceed.

It is important to note that basic authentication is not a secure way of transmitting sensitive information, as the credentials are sent in plain text. It is recommended to use other forms of authentication, such as OAuth, for more secure communication.

## See Also

- [Gleam `http` module documentation](https://gleam.run/documentation/stdlib/http/)
- [Gleam `auth` module documentation](https://gleam.run/documentation/stdlib/auth/)
- [OWASP Authentication Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Authentication_Cheat_Sheet.html)