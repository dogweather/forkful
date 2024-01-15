---
title:                "发送一个http请求"
html_title:           "Rust: 发送一个http请求"
simple_title:         "发送一个http请求"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 为什么

在我们日常的网络使用中，经常会涉及到向服务器发送HTTP请求。而Rust就是一个能够高效地执行这种任务的编程语言。不仅如此，使用Rust能够帮助我们更好地处理网络请求的错误和异常，提高系统的稳定性。

## 如何做到

首先，我们需要引入Rust的[reqwest](https://docs.rs/reqwest/)库来处理HTTP请求。接下来，使用以下代码来创建一个简单的GET请求：

```Rust
let response = reqwest::blocking::get("https://example.com")?; 
```

在上面的代码中，"https://example.com"是我们想要请求的URL。在这种情况下，我们使用了blocking方法来同步执行请求，并且使用了?操作符来处理可能的错误。你也可以使用异步方法来发送HTTP请求，只需要将blocking替换成async。

接下来，我们可以使用response对象来检查请求是否成功，并获取返回的数据：

```Rust
if response.status().is_success() {
    let text = response.text()?;
    println!("{}", text);
}
```

使用response.status()方法可以获取请求的状态码，并通过is_success()方法来检查是否为200。如果是200，则继续获取返回的文本数据，并打印出来。

如果需要发送带有数据的POST请求，可以这样做：

```Rust
let client = reqwest::blocking::Client::new();
let resp = client.post("https://example.com")
    .header("Content-Type", "application/json")
    .body("{\"name\": \"John\", \"age\": 30}")
    .send()?;
```

在上面的代码中，我们使用Client来创建一个POST请求，并指明请求的URL和请求头类型为JSON。接下来，使用.body方法来设置请求的主体内容，并最终使用.send方法来发送请求。

## 深入探讨

在Rust中，发送HTTP请求的方法并不局限于reqwest库。它还可以使用标准库中的[`reqwest::Client`](https://doc.rust-lang.org/std/net/struct.Ipv4Addr.html)来发送请求。此外，还可以使用第三方库如[hyper](https://crates.io/crates/hyper)来做同样的事情。如果你想进一步探索Rust与HTTP请求相关的内容，可以点击以下链接：

- [Rust与HTTP请求](https://rust-lang-nursery.github.io/rust-cookbook/web/clients.html)
- [使用Rust构建RESTful API](https://auth0.com/blog/building-and-testing-a-rest-api-in-rust/)
- [Rust中的HTTP异步编程](https://tokio.rs/tokio/tutorial/async)

## 参考链接

- [Rust文档](https://doc.rust-lang.org/std/net/struct.Ipv4Addr.html)
- [Rust语言参考](https://www.rust-lang.org/zh-CN/learn)
- [reqwest库](https://docs.rs/reqwest/)
- [hyper库](https://crates.io/crates/hyper)