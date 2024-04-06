---
date: 2024-01-20 18:02:47.845181-07:00
description: "\u600E\u4E48\u505A\uFF1A \u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\
  \u7684HTTP\u8BF7\u6C42\u662F\u4E00\u79CD\u5F88\u53E4\u8001\u7684\u8BA4\u8BC1\u65B9\
  \u5F0F\uFF0C\u57FA\u4E8EHTTP/1.0\u3002\u6BD4\u8D77\u73B0\u4EE3\u65B9\u6CD5\uFF08\
  \u5982OAuth 2.0\uFF09\uFF0C\u5B83\u7B80\u5355\u76F4\u63A5\u4F46\u4E0D\u591F\u5B89\
  \u5168\uFF0C\u56E0\u4E3A\u7528\u6237\u540D\u548C\u5BC6\u7801\u662F\u4EE5Base64\u7F16\
  \u7801\u53D1\u9001\u7684\uFF0C\u4F46\u672A\u52A0\u5BC6\u3002 \u8FD9\u4E2A\u8BA4\u8BC1\
  \u65B9\u6CD5\u7684\u66FF\u4EE3\u65B9\u6848\u6709\u5F88\u591A\uFF0C\u5176\u4E2D\u6700\
  \u5E38\u89C1\u7684\u662F\u4EE4\u724C (token)\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:00.726237-06:00'
model: gpt-4-1106-preview
summary: "\u8FD9\u4E2A\u8BA4\u8BC1\u65B9\u6CD5\u7684\u66FF\u4EE3\u65B9\u6848\u6709\
  \u5F88\u591A\uFF0C\u5176\u4E2D\u6700\u5E38\u89C1\u7684\u662F\u4EE4\u724C (token)\
  \ \u57FA\u7840\u7684\u8BA4\u8BC1\u5982Bearer\u8BA4\u8BC1\u3002\u5728\u4E00\u4E9B\
  \u66F4\u52A0\u5B89\u5168\u7684\u573A\u5408\uFF0C\u53EF\u80FD\u4F1A\u7528\u5230\u66F4\
  \u590D\u6742\u7684\u8BA4\u8BC1\u673A\u5236\uFF0C\u4F8B\u5982OAuth2\u3002"
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001 HTTP \u8BF7\u6C42"
weight: 45
---

## 怎么做：
```Rust
use reqwest;
use base64;
use std::error::Error;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    // 用户名和密码
    let username = "user";
    let password = "pass";

    // 编码认证信息
    let auth = format!("{}:{}", username, password);
    let encoded_auth = format!("Basic {}", base64::encode(auth));

    // 创建请求客户端
    let client = reqwest::Client::new();

    // 发送带有基本认证的GET请求
    let response = client.get("http://example.com/resource")
        .header(reqwest::header::AUTHORIZATION, encoded_auth)
        .send()
        .await?;

    // 解析响应
    let status = response.status();
    let body = response.text().await?;

    println!("Status: {}", status);
    println!("Body:\n{}", body);

    Ok(())
}
```
执行上面的代码，如果认证成功，会打印如下输出：
```
Status: 200 OK
Body:
这是受保护的资源内容。
```

## 深入了解：
发送带有基本认证的HTTP请求是一种很古老的认证方式，基于HTTP/1.0。比起现代方法（如OAuth 2.0），它简单直接但不够安全，因为用户名和密码是以Base64编码发送的，但未加密。

这个认证方法的替代方案有很多，其中最常见的是令牌 (token) 基础的认证如Bearer认证。在一些更加安全的场合，可能会用到更复杂的认证机制，例如OAuth2。

在Rust中实现时，我们通常用像`reqwest`这样的库，因为它提供了丰富的功能来简化HTTP请求过程。在通过HTTP发送认证信息前，使用`base64`库对其编码是标准做法。

## 另请参阅：
- Rust `reqwest` 库官方文档: [https://docs.rs/reqwest](https://docs.rs/reqwest)
- HTTP认证基础: [https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- Rust `base64` 库官方文档: [https://docs.rs/base64](https://docs.rs/base64)
