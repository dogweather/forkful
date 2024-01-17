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

## What & Why?

Downloading a web page refers to the process of retrieving the contents of a webpage from the internet and storing it on a local device. Programmers often do this to access and extract specific information from the webpage, such as text, images, or data. This allows them to use the acquired information in their own applications or projects.

## How to:

To download a web page in Rust, we can use the standard library's "reqwest" crate, which provides the necessary functions and methods for making HTTP requests and receiving responses.

```
Rust
use reqwest;
use std::fs::File;
use std::io::prelude::*;

fn main() -> Result<(), Box<dyn std::error::Error>> {
        // make a GET request to the specified URL
        let response = reqwest::blocking::get("https://www.example.com")?;
        // save the response body to a file
        let mut file = File::create("example.html")?;
        response.copy_to(&mut file)?;
        Ok(())
}
```

Running this code will create a file named "example.html" in the local directory and save the contents of the webpage to it.

## Deep Dive

Before the "reqwest" crate, the most commonly used library for HTTP requests in Rust was "hyper", which is still a widely used alternative. However, "hyper" requires more manual configuration and handling, whereas "reqwest" aims to provide a simpler interface for making requests.

When making a GET request with "reqwest", the response is automatically decompressed if it is gzip or deflate encoded. It also handles redirects and follows them automatically.

To improve performance, "reqwest" also supports async/await functionality, making it easier to asynchronously make multiple requests at once.

## See Also

- [reqwest GitHub page](https://github.com/seanmonstar/reqwest)
- [hyper GitHub page](https://github.com/hyperium/hyper)
- [Rust standard library documentation](https://doc.rust-lang.org/std/)