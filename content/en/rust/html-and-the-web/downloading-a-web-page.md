---
aliases:
- /en/rust/downloading-a-web-page/
date: 2024-01-20 17:44:47.158598-07:00
description: "Downloading a web page means grabbing the data it contains. Programmers\
  \ do this to get information, automate tests, scrape data, or check site\u2026"
lastmod: 2024-02-18 23:09:10.847300
model: gpt-4-1106-preview
summary: "Downloading a web page means grabbing the data it contains. Programmers\
  \ do this to get information, automate tests, scrape data, or check site\u2026"
title: Downloading a web page
---

{{< edit_this_page >}}

## What & Why?

Downloading a web page means grabbing the data it contains. Programmers do this to get information, automate tests, scrape data, or check site availability.

## How to:

Let's download a web page using Rust's `reqwest` crate, which provides a simple, asynchronous API for making HTTP requests.

First, add `reqwest` and `tokio` to your `Cargo.toml`:

```toml
[dependencies]
reqwest = "0.11"
tokio = { version = "1", features = ["full"] }
```

Now, in your Rust code:

```rust
use reqwest;
use tokio;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let url = "http://example.com";
    let res = reqwest::get(url).await?;

    let body = res.text().await?;
    println!("Body:\n{}", body);

    Ok(())
}
```

Sample output might look like this, though actual content would vary:

```
Body:
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</body>
</html>
```

## Deep Dive

The `reqwest` crate is one of the most straightforward ways to download web content in Rust. It's evolved from earlier HTTP libraries, providing both synchronous and asynchronous interfaces.

Alternatives include lower-level libraries like `hyper` (which `reqwest` itself uses under the hood), or using `curl` bindings for Rust.

Key implementation steps for downloading a page include making an HTTP GET request and processing the response. Asynchronous programming with `tokio` means your app stays responsive while the network operation completes.

## See Also:

- [`reqwest` documentation](https://docs.rs/reqwest/)
- [`tokio` documentation](https://docs.rs/tokio/)
- [Rust `async`/`await` book](https://rust-lang.github.io/async-book/)
- [MDN web docs on HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP)
