---
title:                "Sending an http request"
html_title:           "Rust recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request is a way for programs to communicate with web servers. This is commonly done in order to retrieve data or perform actions on a remote server.

Programmers use HTTP requests to create applications that interact with external services and APIs. This allows for a wide range of functionalities, such as retrieving information from websites or sending data to a server for processing.

## How to:

Sending an HTTP request in Rust is made easy with the `reqwest` crate, which provides a simple and flexible API for making HTTP requests. Here's an example of how to send a GET request and print the response using the `reqwest` crate:

```Rust
use reqwest;

let response = reqwest::get("https://www.example.com")?
 .text()?;
println!("Response: {}", response);
```

This code snippet creates a `GET` request to the URL `https://www.example.com` and uses the `text()` method to retrieve the response body as a string. 

## Deep Dive

HTTP (Hypertext Transfer Protocol) is the underlying protocol used by the World Wide Web. It was first introduced in 1991 and has evolved over the years to become the standard for client-server communication.

There are different types of HTTP requests, such as `GET`, `POST`, `PUT`, and `DELETE`, each serving a specific purpose. These requests can carry additional data, known as headers and body, to provide more information to the server.

Other than the `reqwest` crate, there are alternative libraries in Rust for making HTTP requests, such as `hyper` and `ureq`. These provide more low-level control but may require more code to achieve the same functionality as `reqwest`.

Behind the scenes, `reqwest` uses the `hyper` crate to handle the underlying network communication. It also supports features like asynchronous requests and HTTPS.

## See Also

- [Official `reqwest` Documentation](https://docs.rs/reqwest/0.11.6/reqwest/)
- [HTTP Requests in Rust - A Simple Tutorial](https://blog.logrocket.com/http-requests-in-rust-a-simple-tutorial/)
- [Introduction to HTTP - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)