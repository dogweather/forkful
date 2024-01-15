---
title:                "बेसिक प्रमाणीकरण के साथ एक एचटीटीपी अनुरोध भेजना"
html_title:           "Rust: बेसिक प्रमाणीकरण के साथ एक एचटीटीपी अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ एक एचटीटीपी अनुरोध भेजना"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Kyun

HTTP request ko basic authentication ke saath bhejna kisi bhi Rust programmer ke liye bahut zaroori hai. Basic authentication request se server ko user ke identity ko verify karne ki permission milti hai.

## Kaise Karein

Request bhejne ke liye, hum `reqwest` library ka istemaal karenge. Sabse pehle, hum is library ko `Cargo.toml` file mein add karenge:

```Rust
[dependencies]
reqwest = { version = "0.11", features = ["blocking", "json"] }
```

Phir, hum apni `main.rs` file mein `reqwest` library ko import karenge:

```Rust
use reqwest::blocking::{Client, Response};
```

Ab hum `Client` instance banaenge jisse hum HTTP request bhejenge:

```Rust
let client = Client::new();
```

Uske baad, hum `RequestBuilder` ko bhi banaenge jisme hum HTTP URL aur method specify karenge:

```Rust
let request_builder = client.request(Method::GET, "https://www.example.com");
```

Ab hum `Authorization` header ko specify karenge jisse hum basic authentication credentials send kar sakte hain:

```Rust
let auth_header_value = format!("Basic {}:{}", username, password);
request_builder.header(AUTHORIZATION, auth_header_value);
```

Finally, hum HTTP request ko bhej sakte hain aur server se response ko receive kar sakte hain:

```Rust
let response = request_builder.send()?;
let status = response.status();
let body = response.text()?;
```

## Gehri Jaankari

Basic authentication mein, hum username aur password ko base64 encoding ke through send karte hain. Server phir in credentials ko decode karke verify karta hai. Agar credentials sahi hain, server `200 OK` status code ke saath response bhejta hai.

## See Also

- [Rust reqwest library documentation](https://docs.rs/reqwest/0.11.0/reqwest/)
- [HTTP basic authentication](https://www.digitalocean.com/community/tutorials/how-to-use-basic-authentication-with-http-methods-in-rust) (Rust mein)