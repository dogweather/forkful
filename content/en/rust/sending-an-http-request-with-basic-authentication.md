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

## Why

Sending HTTP requests with basic authentication is a fundamental part of web development and allows for secure communication between a client and server. It is a necessary step in creating applications that require user authentication.

## How To

To send an HTTP request with basic authentication in Rust, we first need to include the `reqwest` library in our project by adding it as a dependency in `Cargo.toml`.

```Rust
[dependencies]
reqwest = { version = "0.11", features = ["json"] }
```

Next, we need to import the `reqwest` crate into our project.

```Rust
use reqwest;
```

We can now create an HTTP request with basic authentication by using the `reqwest::Client` and `RequestBuilder` structs. We can also specify the basic authentication credentials using the `basic_auth()` method.

```Rust
let client = reqwest::Client::new();
let request = client.get("https://example.com")
    .basic_auth("username", Some("password"));
```

We can then use the `send()` method to actually send the request.

```Rust
let response = request.send();
```

And finally, we can retrieve the response data by using the `text()` or `json()` methods, depending on the format of the response.

```Rust
let body = response.text().unwrap();
```

Here is a complete example of sending an HTTP GET request with basic authentication and retrieving the response body.

```Rust
use reqwest;

fn main() {
    let client = reqwest::Client::new();
    let request = client.get("https://example.com")
        .basic_auth("username", Some("password"));
    let response = request.send();
    let body = response.text().unwrap();
    println!("{}", body);
}
```

The above code will print out the response body in the console.

## Deep Dive

In the above example, we used the `reqwest` library's `basic_auth()` method to specify the basic authentication credentials. This method takes in two parameters: the username and the password, both of which are of type `&str`.

It is important to note that basic authentication is not considered a secure method of authentication as the username and password are sent in plain text. This is why it is recommended to use HTTPS when performing basic authentication.

Additionally, the `reqwest` library also provides other methods for authentication such as `bearer_auth()` for OAuth 2.0 authentication and `digest_auth()` for Digest authentication.

## See Also

For more information on sending HTTP requests with basic authentication in Rust, check out the following resources:

- [The `reqwest` crate documentation](https://docs.rs/reqwest/latest/reqwest/)
- [Using `basic_auth()` in `reqwest` tutorial](https://www.matthias-endler.de/2017/rust-http-client/)
- [HTTP authentication schemes](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)