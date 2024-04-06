---
date: 2024-01-20 18:00:27.162829-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:38:46.678336-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\uFF1A \u53D1\u9001HTTP\u8BF7\u6C42\u7684\u80FD\u529B\u5728\
  Rust\u4E2D\u901A\u5E38\u9700\u8981\u5916\u90E8\u5E93\uFF0C`reqwest`\u5E93\u5728\u793E\
  \u533A\u4E2D\u5E7F\u53D7\u6B22\u8FCE\uFF0C\u56E0\u4E3A\u5B83\u5F02\u6B65\u3001\u5B89\
  \u5168\u5E76\u6613\u4E8E\u4F7F\u7528\u3002\u8BF7\u6C42\u4E4B\u524D\uFF0CInternet\u901A\
  \u4FE1\u7684\u57FA\u7840\u662F\u8BF8\u5982Telnet\u548CFTP\u4E4B\u7C7B\u7684\u534F\
  \u8BAE\u7684\u3002\u5F53Web\u84EC\u52C3\u53D1\u5C55\uFF0CHTTP\u6210\u4E3A\u6807\u5FD7\
  \u6027\u534F\u8BAE\u3002\u73B0\u5982\u4ECA\uFF0C\u4F60\u8FD8\u53EF\u4EE5\u901A\u8FC7\
  \u50CF`hyper`\u8FD9\u6837\u7684\u5E95\u5C42\u5E93\u6765\u53D1\u9001HTTP\u8BF7\u6C42\
  \uFF0C\u5B83\u7ED9\u4F60\u66F4\u591A\u63A7\u5236\u4F46\u7F16\u7801\u66F4\u590D\u6742\
  \u3002\u5E95\u5C42\uFF0CHTTP\u8BF7\u6C42\u7531\u5BA2\u6237\u7AEF\u5411\u670D\u52A1\
  \u5668\u53D1\u9001\u4E00\u4E2A\u683C\u5F0F\u5316\u7684\u6587\u672C\u6D88\u606F\u6784\
  \u6210\uFF0C\u7136\u540E\u670D\u52A1\u5668\u4EE5\u4E00\u4E2A\u683C\u5F0F\u5316\u7684\
  \u6D88\u606F\u56DE\u590D\u3002"
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
