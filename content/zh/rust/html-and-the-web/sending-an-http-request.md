---
date: 2024-01-20 18:00:27.162829-07:00
description: "\u5982\u4F55\uFF1A ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.516813-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u53D1\u51FA HTTP \u8BF7\u6C42"
weight: 44
---

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
