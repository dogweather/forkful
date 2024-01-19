---
title:                "发送http请求"
html_title:           "C#: 发送http请求"
simple_title:         "发送http请求"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

HTTP请求是网络编程中的一种通信方式，通常用于获取网络服务器的数据。程序员之所以使用HTTP请求，主要是为了实现客户端与服务器的数据交换。

## 如何做：

我们将使用`reqwest`库来示例如何在Rust发送HTTP请求。首先，添加`reqwest`和`tokio`以支持异步操作：

```Rust
[dependencies]
reqwest = "0.11"
tokio = { version = "1", features = ["full"] }
```

然后编写一个简单的函数来发送GET请求：

```Rust
use reqwest::Error;

#[tokio::main]
async fn main() -> Result<(), Error> {
    let res = reqwest::get("http://www.example.com").await?;
    println!("{}", res.text().await?);
    Ok(())
}
```

运行你的程序，你将看到来自"http://www.example.com"的响应内容。

## 深入探索

发送HTTP请求的需求在早期网络应用发展中就已经出现。在那时，LINEMODE和TELNET等底层协议被广泛使用，复杂且不易使用。后来随着Web的诞生，HTTP协议成为了标准，为网络编程带来了重大革新。

在Rust中，你也可以选择其他方案，例如`hyper`和`surf`等库。它们都有其优缺点，你可以根据具体用途选择最合适的。

上面的示例很简单，适合入门，但在实际应用中，我们可能需要设置请求头、使用POST方法发送数据等，这就需要深入了解`reqwest`库的API。

## 另请参阅

1. [reqwest库官方文档](https://docs.rs/reqwest/0.11.7/reqwest/)
2. [HTTP协议详解](https://developer.mozilla.org/zh-CN/docs/Web/HTTP)
3. [Rust异步编程简介](https://rust-lang.github.io/async-book/)
4. [其他HTTP客户端库：hyper](https://hyper.rs/)
5. [其他HTTP客户端库：surf](https://docs.rs/surf/2.3.1/surf/)