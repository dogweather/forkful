---
date: 2024-01-20 18:00:37.393100-07:00
description: "How to: \u041F\u043E\u0433\u043B\u0438\u0431\u043B\u0435\u043D\u0438\
  \u0439 \u0440\u043E\u0437\u0433\u043B\u044F\u0434: HTTP requests have been fundamental\
  \ to web communication since Tim Berners-Lee's first browser. Alternatives like\
  \ gRPC are\u2026"
lastmod: '2024-04-05T22:51:02.059463-06:00'
model: gpt-4-1106-preview
summary: "\u041F\u043E\u0433\u043B\u0438\u0431\u043B\u0435\u043D\u0438\u0439 \u0440\
  \u043E\u0437\u0433\u043B\u044F\u0434."
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443"
weight: 44
---

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
