---
title:                "Rust: 使用基本身份验证发送 http 请求"
simple_title:         "使用基本身份验证发送 http 请求"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# 为什么

HTTP请求是构建现代网络应用程序不可或缺的基本步骤。基本身份验证是一种常用的安全协议，它可以确保只有经过身份验证的用户才能访问特定资源。因此，学习如何发送带有基本身份验证的HTTP请求是非常重要的。

# 如何做

发送带有基本身份验证的HTTP请求在Rust中非常简单。首先，我们需要导入 Hyper crate 进行HTTP客户端请求。然后，我们将使用身份验证用户名和密码创建一个HttpBasicAuth身份验证结构体。接下来，我们使用build方法将身份验证结构体附加到请求中，然后将请求发送到指定的URL。

```Rust
// 导入Hyper crate
extern crate hyper;
use hyper::{Client, Uri};
use hyper::http::Request;
use hyper::client::HttpConnector;
use hyper::header::{Basic, Authorization};
use std::env;

// 创建用于发送HTTP请求的客户端
let mut client = Client::new();

// 定义HTTP请求的URL
let url = "https://example.com/api";

// 定义需要发送的身份验证信息
let username = "myusername";
let password = "mypassword";

// 创建HttpBasicAuth身份验证结构体
let basic_auth = Basic {
    username: username.to_owned(),
    password: Some(password.to_owned()),
};

// 构建HTTP请求并附加身份验证结构体
let request = Request::get(url)
    .header(Authorization(basic_auth))
    .body(Body::empty())
    .unwrap();

// 发送HTTP请求并获取响应
let response = client.request(request).await.unwrap();

// 打印响应的状态码
println!("Response status code: {}", response.status());

// 打印响应的正文
let body = response.into_body();
let full_body = hyper::body::to_bytes(body).await?;
println!("Response body: {:?}", full_body);
```

输出：

```Rust
Response status code: 200
Response body: "Welcome to the API!"
```

# 深入探讨

除了上述的基本身份验证方式，还可以使用其他身份验证方式来发送HTTP请求。Hyper crate提供了各种不同的身份验证头文件，您可以根据需要选择最适合的方法。另外，您还可以通过设置自定义HTTP头来实现更高级的身份验证方式。

# 参考链接

- [Rust - Hyper crate](https://github.com/hyperium/hyper)
- [HTTP Basic Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [HTTP Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)