---
title:                "Sending an http request with basic authentication"
html_title:           "Fish Shell recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Sending an HTTP Request with Basic Authentication in Rust

## What & Why?

Sending an HTTP request with basic authentication involves transmitting data to a server, with user credentials for access. It aids communication with APIs, data fetching, and site scraping—whenever access permission is required.

## How to:

In Rust, the `reqwest` crate is the go-to for HTTP requests. For basic authentication, it's straightforward:

Install `reqwest` by adding this into your `Cargo.toml`:

```Rust
[dependencies]
reqwest = { version = "0.11", features = ["blocking", "json"] }
```

Then use your credentials while building the HTTP request. Here's an example:

```Rust
use reqwest::blocking::Client;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let client = Client::new();

    let res = client.get("https://www.yourwebsite.com")  
        .basic_auth("your_username", Some("your_password"))  
        .send()?;

    println!("Status: {}", res.status());
    let text: String = res.text()?;
    println!("Body: {}", text);
    
    Ok(())
}
```

## Deep Dive

Historically, basic authentication has been the simplest method to transmit credentials over HTTP. It sends credentials as Base64 encoded strings which is far from secure and is becoming outdated. Modern alternatives include Digest Authentication, OAuth, and JWT tokens.

In Rust, `reqwest` is a high-level HTTP client which makes these tasks easier. It helps by abstracting the underlying HTTP protocol details. Remember that basic authentication might not be secure for sensitive data even when using HTTPS, as it uses Base64 encoded strings.

## See Also

1. More on `reqwest` in Rust: [https://docs.rs/reqwest/](https://docs.rs/reqwest/)
2. More on Basic Authentication: [https://tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)
3. Alternatives to Basic Authentication: [https://datatracker.ietf.org/doc/html/rfc8270](https://datatracker.ietf.org/doc/html/rfc8270)