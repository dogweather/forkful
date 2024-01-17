---
title:                "Sending an http request with basic authentication"
html_title:           "Rust recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request with basic authentication is a way for programmers to ensure secure communication between a client and server. It involves including a username and password in the request header, which is then checked by the server to allow access. This is essential for protecting sensitive data and controlling access to web resources.

## How to:

Sending an HTTP request with basic authentication in Rust is straightforward. First, import the reqwest crate, which provides a convenient interface for making HTTP requests. Then, use the `basic_auth` function to create a request with a username and password. Finally, send the request using the `send` method and handle the response. Here's an example:

```Rust
use reqwest;

let username = "user123";
let password = "pass456";

let client = reqwest::Client::new();
let response = client
    .get("https://example.com")
    .basic_auth(username, password)
    .send()
    .await?;

println!("Status: {}", response.status());

let body = response.text().await?;
println!("Body: {}", body);
```

Output:

```
Status: 200 OK
Body: Hello, world!
```

## Deep Dive:

Basic authentication has been around since the early days of the internet and is still widely used today. It was originally designed for use in simple client-server architectures, but it has been adapted for use in modern web applications. Other authentication methods, such as OAuth, are now more commonly used due to their added security features, but basic authentication remains a simple and reliable option.

In addition to using the `basic_auth` function, the reqwest crate also provides the `basic_auth_header` function, which can be used to manually create the authorization header if needed. This can be useful for custom authentication schemes or for integrating with existing systems.

## See Also:

- [Rust reqwest crate documentation](https://docs.rs/reqwest)
- [HTTP Basic Authentication on MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)