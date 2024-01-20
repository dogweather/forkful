---
title:                "Downloading a web page"
html_title:           "Bash recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Downloading a Web Page Using Rust

## What & Why?

Downloading a web page means capturing its contents as an offline file. Programmers download web pages to extract data, test site performance, or archive content.

## How to:

First, you'll need to add the reqwest library to your `Cargo.toml` file:

```Rust
[dependencies]
reqwest = "0.10.9"
tokio = {version = "^0.2", features = ["full"]}
```

Then, use the `reqwest` and `tokio` libraries to fetch a web page:

```Rust
use reqwest::Error;

#[tokio::main]
async fn main() -> Result<(), Error> {
    let response = reqwest::get("https://www.rust-lang.org/")
        .await?
        .text()
        .await?;

    println!("{}", response);

    Ok(())
}
```

This basic example fetches the HTML content of "https://www.rust-lang.org/" and prints it to the console.

## Deep Dive

Historically programmers used `wget` or `curl` command-line tools to download HTML content. But for modern tasks, tools like `reqwest` in Rust provide more flexibility and power.

Alternatives to `reqwest` include the lower-level `hyper` and `surf` libraries. These can provide more control over HTTP requests, but at the cost of added complexity.

When it comes to the nitty-gritty, `reqwest::get` fetches a webpage by making a GET HTTP request. The `await` calls are needed as these operations are asynchronous - they can run at the same time as other parts of your program.

## See Also

- [Reqwest documentation](https://docs.rs/reqwest)
- [Tokio documentation](https://docs.rs/tokio/)
- [Hyper library](https://hyper.rs/)
- [Surf library introduction](https://rust-lang-nursery.github.io/rust-cookbook/web/scraping.html)