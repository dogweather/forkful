---
title:                "Downloading a web page"
html_title:           "Rust recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why
Downloading web pages is a common task for web developers, data analysts, and anyone looking to extract information from a website. Using Rust, a fast and reliable programming language, can make this process even smoother.

## How To
To download a web page in Rust, we can make use of the `reqwest` crate, which provides a simple and user-friendly interface for making HTTP requests. First, we need to add the `reqwest` crate as a dependency in our `Cargo.toml` file:

```Rust
[dependencies]
reqwest = { version = "0.11.3", features = ["blocking"] }
```

Next, we can use the `reqwest::get()` function to make a `GET` request to the desired URL. This function returns a `Response` object, which we can then use to access the response body:

```Rust
use reqwest::blocking::get;

let response = get("https://www.example.com").unwrap();
let body = response.text().unwrap();

println!("{}", body); // prints the downloaded web page
```

We can also specify additional request options, such as headers or query parameters, by creating a `Client` object and using it to make the request:

```Rust
use reqwest::blocking::{get, Client};

let client = Client::new(); // create a Client object
let response = client.get("https://www.example.com")
    .header("User-Agent", "My Rust Application")
    .query(&[("search", "rust")])
    .send()
    .unwrap();

let body = response.text().unwrap();

println!("{}", body); // prints the downloaded web page
```

## Deep Dive
Under the hood, the `reqwest` crate makes use of the `hyper` crate, which provides a low-level HTTP implementation in Rust. By default, `reqwest` uses a blocking client, meaning the current thread will be blocked until the request is completed. However, the `reqwest` crate also provides an asynchronous client for more efficient handling of multiple requests. Additionally, the `reqwest` crate supports cookie management, redirection, and other useful features for web scraping tasks.

## See Also
- [reqwest crate documentation](https://docs.rs/reqwest/)
- [hyper crate documentation](https://docs.rs/hyper/)
- [Official Rust website](https://www.rust-lang.org/)