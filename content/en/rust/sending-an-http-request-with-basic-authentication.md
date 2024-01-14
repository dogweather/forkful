---
title:                "Rust recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why

Sending an HTTP request with basic authentication is a common task in web development. It allows for secure communication between a client and server by adding a username and password to the request.

## How To

To demonstrate how to send an HTTP request with basic authentication in Rust, we will be using the `reqwest` crate. First, let's create a `Client` and specify our authentication credentials:

```Rust
let mut client = reqwest::Client::new();
let credentials = reqwest::basic_auth("username", Some("password"));
```

Next, we can use the `get` method to specify the URL we want to send the request to. We can also add any necessary headers or parameters to our request before sending it:

```Rust
let response = client.get("https://www.example.com")
    .header("Content-Type", "application/json")
    .bearer_auth("token123")
    .basic_auth(credentials)
    .send()
    .await?
```

Finally, we can use the `text` method to get the response body as a string, or the `json` method to deserialize the response into a specific data type:

```Rust
let text = response.text().await?;
let data: User = response.json().await?;
println!("Response body: {}", text);
println!("User ID: {}", data.id);
```

The output of our request might look something like this:

```
Response body: Welcome to example.com!
User ID: 12345
```

## Deep Dive

Behind the scenes, sending an HTTP request with basic authentication involves encoding the username and password in a specific format known as Base64. This encoding is then added to the `Authorization` header of the request.

It is important to note that basic authentication is not the most secure method of authentication, as the username and password can be easily decoded if intercepted. It is recommended to use other forms of authentication, such as OAuth, whenever possible.

## See Also

Here are some other helpful resources for working with HTTP requests in Rust:

- [Reqwest documentation](https://docs.rs/reqwest/latest/reqwest/index.html)
- [Sending HTTP requests with Rust: a complete example](https://blog.logrocket.com/sending-http-requests-in-rust/)
- [Rust Cookbook: Performing HTTP requests](https://rust-lang-nursery.github.io/rust-cookbook/web/clients.html)