---
title:                "发出 HTTP 请求"
date:                  2024-01-20T18:00:27.162829-07:00
model:                 gpt-4-1106-preview
simple_title:         "发出 HTTP 请求"

category:             "Rust"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 什么及为什么？
在编程中，发送HTTP请求是与Web服务进行交互的方式。（1）这能让你的应用获取数据、发送数据，或与远程服务器通信。（2）程序员这么做是为了让应用能与互联网上的其他服务集成。

## 如何：
```Rust
// 引入依赖库
use reqwest;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    // 发送GET请求
    let response = reqwest::get("https://httpbin.org/get").await?;

    // 确认我们得到了成功的状态码
    assert!(response.status().is_success());

    // 解析响应体为文本
    let body = response.text().await?;

    println!("响应体: {}", body);
    Ok(())
}
```
输出样例：
```
响应体: {
  "args": {}, 
  "headers": {
    ...
  }, 
  "origin": "your.ip", 
  "url": "https://httpbin.org/get"
}
```

## 深入探索
发送HTTP请求的能力在Rust中通常需要外部库，`reqwest`库在社区中广受欢迎，因为它异步、安全并易于使用。请求之前，Internet通信的基础是诸如Telnet和FTP之类的协议的。当Web蓬勃发展，HTTP成为标志性协议。现如今，你还可以通过像`hyper`这样的底层库来发送HTTP请求，它给你更多控制但编码更复杂。底层，HTTP请求由客户端向服务器发送一个格式化的文本消息构成，然后服务器以一个格式化的消息回复。

## 另见
- [reqwest库文档](https://docs.rs/reqwest/)
- [异步Rust图书](https://rust-lang.github.io/async-book/)
- [HTTP官方规范](https://httpwg.org/specs/rfc7231.html)
