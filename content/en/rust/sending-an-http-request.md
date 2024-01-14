---
title:                "Rust recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why
Rust is a popular programming language known for its speed, safety, and concurrency. It is commonly used for system programming, web development, and game development. One of the key tasks in web development is sending HTTP requests to fetch data from servers. In this blog post, we will explore how to send HTTP requests in Rust and see why it's a useful skill to have in your programming arsenal.

## How To

Sending an HTTP request in Rust involves a few simple steps:

1. Import the reqwest crate: ```Rust
use reqwest;
```

2. Create a Client object: ```Rust
let client = reqwest::Client::new();
```

3. Use the ```get()``` function to create a GET request: ```Rust
let response = client.get("https://example.com").send();
```

4. Handle the response by checking for success and printing the body: ```Rust
if response.is_ok() {
    println!("Response: {}", response.unwrap().text().unwrap());
} else {
    println!("Request failed.")
}
```

Let's break down the code with an example. Suppose we want to send a GET request to the website http://www.randomword.com/ and print out the word of the day. Here's what the code would look like:

```Rust
use reqwest;

let client = reqwest::Client::new();
let response = client.get("https://www.randomword.com/word").send();

if response.is_ok() {
    let word = response.unwrap().text().unwrap();
    println!("Word of the day: {}", word);
} else {
    println!("Request failed.")
}
```

When we run this code, the output would be:

```
Word of the day: serendipity
```

As you can see, sending an HTTP request in Rust is a straightforward process. You can also customize the request headers, add query parameters, or even send POST requests using the same ```get()``` function.

## Deep Dive

Under the hood, Rust uses the Hyper library to handle HTTP requests. The ```get()``` function returns a ```Result``` type that contains a response object with information such as status code, headers, and body. We can access this information using the respective methods. For example, to get the status code of the response, we can use ```response.status()``` and to get the headers, we can use ```response.headers()```.

In addition to the reqwest crate, there are also other HTTP client libraries available in Rust, such as Surf and Isahc. Each library has its own unique features, and it's worth exploring them to find the best fit for your project.

## See Also

- [Official Rust website](https://www.rust-lang.org/)
- [Reqwest crate documentation](https://docs.rs/reqwest/latest/reqwest/)
- [Hyper library documentation](https://docs.rs/hyper/latest/hyper/)
- [Surf crate documentation](https://docs.rs/surf/latest/surf/)
- [Isahc crate documentation](https://docs.rs/isahc/latest/isahc/)