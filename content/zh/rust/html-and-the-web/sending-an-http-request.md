---
aliases:
- /zh/rust/sending-an-http-request/
date: 2024-01-20 18:00:27.162829-07:00
description: "\u5728\u7F16\u7A0B\u4E2D\uFF0C\u53D1\u9001HTTP\u8BF7\u6C42\u662F\u4E0E\
  Web\u670D\u52A1\u8FDB\u884C\u4EA4\u4E92\u7684\u65B9\u5F0F\u3002\uFF081\uFF09\u8FD9\
  \u80FD\u8BA9\u4F60\u7684\u5E94\u7528\u83B7\u53D6\u6570\u636E\u3001\u53D1\u9001\u6570\
  \u636E\uFF0C\u6216\u4E0E\u8FDC\u7A0B\u670D\u52A1\u5668\u901A\u4FE1\u3002\uFF082\uFF09\
  \u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u8BA9\u5E94\u7528\u80FD\u4E0E\
  \u4E92\u8054\u7F51\u4E0A\u7684\u5176\u4ED6\u670D\u52A1\u96C6\u6210\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:58.938390
model: gpt-4-1106-preview
summary: "\u5728\u7F16\u7A0B\u4E2D\uFF0C\u53D1\u9001HTTP\u8BF7\u6C42\u662F\u4E0EWeb\u670D\
  \u52A1\u8FDB\u884C\u4EA4\u4E92\u7684\u65B9\u5F0F\u3002\uFF081\uFF09\u8FD9\u80FD\u8BA9\
  \u4F60\u7684\u5E94\u7528\u83B7\u53D6\u6570\u636E\u3001\u53D1\u9001\u6570\u636E\uFF0C\
  \u6216\u4E0E\u8FDC\u7A0B\u670D\u52A1\u5668\u901A\u4FE1\u3002\uFF082\uFF09\u7A0B\u5E8F\
  \u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u8BA9\u5E94\u7528\u80FD\u4E0E\u4E92\u8054\
  \u7F51\u4E0A\u7684\u5176\u4ED6\u670D\u52A1\u96C6\u6210\u3002"
title: "\u53D1\u51FA HTTP \u8BF7\u6C42"
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
