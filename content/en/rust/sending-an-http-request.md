---
title:                "Sending an http request"
html_title:           "Rust recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why

If you're a web developer or creating networked applications, chances are you'll need to send HTTP requests. The HTTP protocol is the backbone of communication on the web, allowing for the transfer of data between servers and clients.

## How To

Sending an HTTP request in Rust is made simple with the Hyper crate. First, add the crate to your dependencies in the Cargo.toml file:

```Rust
[dependencies]
hyper = "0.14.2"
```

Next, import the necessary modules in your code:

```Rust
use hyper::Client;
use hyper::body::HttpBody;
```

Then, create a new HTTP client and specify the URL you want to send the request to:

```Rust
let client = Client::new();
let url = "https://www.example.com";
```

Now, you can create an HTTP request using the `get()` method and passing in the URL:

```Rust
let request = client.get(url);
```

Next, send the request using the `send()` method and store the response in a variable:

```Rust
let mut response = client.send(request).await.unwrap();
```

To access the response body, you can use the `body_bytes()` method and print it to the console:

```Rust
let mut body = Vec::new();
while let Some(chunk) = response.body_mut().data().await {
    let data = chunk.unwrap();
    body.extend_from_slice(&data);
}
println!("Response body: {:?}", std::str::from_utf8(&body).unwrap());
```

## Deep Dive

Hyper is built on top of the Tokio asynchronous runtime, allowing for non-blocking and efficient sending of HTTP requests. This means that your code can continue to run while waiting for a response, making it great for creating high-performance web applications.

Additionally, Hyper allows for easy customization of headers and request parameters, giving you full control over your HTTP requests. It also supports different HTTP methods like POST, PUT, and DELETE, allowing you to create more complex interactions with web servers.

## See Also

- [Hyper documentation](https://docs.rs/hyper/0.14.3/hyper/)
- [Tokio documentation](https://docs.rs/tokio/1.7.1/tokio/)

Sending HTTP requests in Rust is a useful skill to have in your developer toolkit. With the Hyper crate, you can easily make efficient and customizable requests, making your web development projects more robust and powerful. Happy coding!