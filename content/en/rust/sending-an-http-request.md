---
title:                "Sending an http request"
html_title:           "Bash recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request is, in essence, demanding data from a server, website, or other service via the Hypertext Transfer Protocol (HTTP). Programmers use it to interact with web services, fetch information, and communicate with other systems.

## How to:

In Rust, you can use libraries like `reqwest` to perform HTTP requests. Firstly, make sure you've added `reqwest` to your `Cargo.toml`:

```Rust
[dependencies]
reqwest = "0.11"
```

Now, let's send a GET request:

```Rust
use reqwest::Error;

#[tokio::main]
async fn main() -> Result<(), Error> {
    let response = reqwest::get("https://httpbin.org/ip").await?;
    
    if response.status().is_success() {
        let body = response.text().await?;
        println!("body = {:?}", body);
    } else {
        println!("Error! Response: {:?}", response.status());
    }

    Ok(())
}
```

Here, we use the `get` function to send a request. If the request is successful, we print the response body; otherwise, we print the error status.

## Deep Dive

Historically, HTTP requests were pretty low-level tasks involving manual socket programming. Libraries like Python's `requests` and Rust's `reqwest` brought a layer of user-friendliness to HTTP requests. 

In Rust, alternatives to `reqwest` include `hyper`, a fast and low-level HTTP library, and `surf`, a lightweight, simple library for making requests. Each option trades off between speed, simplicity, and feature-completeness.

Behind the scenes, when you send an HTTP request, `reqwest` handles much of the dirty work such as establishing a connection, formatting the request into HTTP protocol text, sending the request, and then parsing the response.

## See Also

To explore more, check out these resources:

1. Reqwest Documentation - [https://docs.rs/reqwest](https://docs.rs/reqwest)
2. Hyper Documentation - [https://hyper.rs/](https://hyper.rs/)
3. Surf Github - [https://github.com/http-rs/surf](https://github.com/http-rs/surf)
4. HTTP - The Rust Book [https://doc.rust-lang.org/book/ch20-01-single-threaded.html](https://doc.rust-lang.org/book/ch20-01-single-threaded.html)