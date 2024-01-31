---
title:                "Надсилання HTTP-запиту"
date:                  2024-01-20T18:00:37.393100-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
Що таке та навіщо?
Sending an HTTP request is how we ask the internet for data or actions. Programmers do it to interact with web services, fetch information, or send data to servers.

## How to:
Як це зробити:
```Rust
use reqwest; // Add to Cargo.toml: reqwest = "0.11"
use std::error::Error;

#[tokio::main] // Uses async main function with Tokio runtime
async fn main() -> Result<(), Box<dyn Error>> {
    let response = reqwest::get("https://api.example.com/data")
        .await?
        .text()
        .await?;

    println!("Response: {}", response);
    Ok(())
}
```
Sample output:
```
Response: {"key": "value"}
```

## Deep Dive:
Поглиблений розгляд:
HTTP requests have been fundamental to web communication since Tim Berners-Lee's first browser. Alternatives like gRPC are picking up steam for performance reasons but aren't as widespread yet. Knowing HTTP is foundational.

Sending an HTTP request in Rust engages external crates like `reqwest`, which simplifies tasks. Libraries like `hyper` provide lower-level access and more control. Rust's async ecosystem, including `tokio`, makes handling concurrent requests efficient.

## See Also:
Дивіться також:
- [The Rust Programming Language - Async Book](https://rust-lang.github.io/async-book/)
- [Reqwest crate documentation](https://docs.rs/reqwest/)
- [Hyper crate documentation](https://hyper.rs/)
- [Learning HTTP Basics](https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP)
