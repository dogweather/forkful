---
title:                "Http अनुरोध भेजना"
html_title:           "Rust: Http अनुरोध भेजना"
simple_title:         "Http अनुरोध भेजना"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why

Kya aap ek web developer hai aur apne websites par HTTP request bhejne ka kaam karte hai? Ya phir aap ek avid internet user hai aur HTTP request ke baare mein janana chahte hain? Rust programming language aapke liye ek perfect option ho sakta hai. Ismein built-in support ke saath HTTP request bhejna bahut hi aasaan hai.

## How To

```Rust
// Import the necessary libraries
use reqwest::Error;
use reqwest::blocking::Client;

fn main() {
    // Create a client for sending request
    let client = Client::new();

    // Send a GET request to a URL
    let response = client.get("https://www.example.com").send();

    // Print the response status code
    match response {
        Ok(r) => println!("Response status code: {}", r.status()),
        Err(e) => println!("Error occurred: {}", e),
    }
}
```

```
Output:
Response status code: 200 OK
```

## Deep Dive

Rust language mein HTTP request bhejne ke liye hum `reqwest` library ka use karte hai. Isse ek client bana sakte hai jo humein URL par GET, POST, PUT, DELETE request bhejne mein help karta hai. Iske alawa, hum headers, cookies, body data aur authentication bhi add kar sakte hai apne request mein.

## See Also

- [Official Reqwest Documentation](https://docs.rs/reqwest)
- [HTTP Requests in Rust Tutorial](https://dev.to/wackyshenanigans/send-http-requests-in-rust-1jk1)
- [Rust Programming Language Official Website](https://www.rust-lang.org/)