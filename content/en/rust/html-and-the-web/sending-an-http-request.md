---
aliases:
- /en/rust/sending-an-http-request/
date: 2024-01-20 18:00:43.269461-07:00
description: "Sending an HTTP request fetches data from or sends data to a web server.\
  \ Programmers do it to interact with web services or APIs \u2013 grabbing info,\
  \ posting\u2026"
lastmod: 2024-02-18 23:09:10.845415
model: gpt-4-1106-preview
summary: "Sending an HTTP request fetches data from or sends data to a web server.\
  \ Programmers do it to interact with web services or APIs \u2013 grabbing info,\
  \ posting\u2026"
title: Sending an HTTP request
---

{{< edit_this_page >}}

## What & Why?
Sending an HTTP request fetches data from or sends data to a web server. Programmers do it to interact with web services or APIs – grabbing info, posting updates, you name it.

## How to:
To send a GET request in Rust, we use the `reqwest` crate. First, add it to your `Cargo.toml`:

```toml
[dependencies]
reqwest = "0.11"
tokio = { version = "1", features = ["full"] }
```

Now, rustle up some async Rust code:

```rust
use reqwest;
use tokio;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let response_text = reqwest::get("https://api.example.com/data")
        .await?
        .text()
        .await?;
    
    println!("Response: {}", response_text);
    Ok(())
}
```

Sample output might look like this:

```
Response: {"key": "value", "hello": "world"}
```

That's all it takes to hit an endpoint with a GET request!

## Deep Dive
HTTP requests are as old as the hills in internet years. They're the backbone of web-based communication. Rust uses crates like `reqwest` because it's not a web-specific language – flexibility is key. `reqwest` is built on `hyper`, which is fast and low-level, but `reqwest` adds ease of use on top.

Alternatives to `reqwest`? Sure. `hyper` for speed demons, `surf` if you're into async Rust or `ureq` for simplicity – no async fuss needed. 

Under the hood, when you send an HTTP request, Rust's doing much what any language would: establishing a TCP connection, sending along a formatted HTTP request, and interpreting the raw response. Asynchronous handling of these requests is where Rust shines, letting you do other stuff while awaiting the server’s reply.

## See Also
- [reqwest Documentation](https://docs.rs/reqwest/)
- [The Rust Async Book](https://rust-lang.github.io/async-book/)
- [Hyper HTTP Library](https://hyper.rs/)
- [API Guidelines](https://rust-lang.github.io/api-guidelines/)
