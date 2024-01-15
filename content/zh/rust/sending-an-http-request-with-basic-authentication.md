---
title:                "使用基本身份验证发送http请求"
html_title:           "Rust: 使用基本身份验证发送http请求"
simple_title:         "使用基本身份验证发送http请求"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 为什么要使用基本认证发送HTTP请求？

当您需要从受保护的资源中获取数据时，比如从银行或邮件服务器，基本认证是一种常用的安全认证方式。通过发送包含用户名和密码的HTTP请求，您可以验证自己的身份，并获得对资源的访问权限。

## 如何使用Rust发送带有基本认证的HTTP请求？

您可以使用Rust的reqwest库来发送HTTP请求，并使用基本认证来验证身份。首先，您需要在Cargo.toml文件中添加reqwest依赖项，并导入reqwest库。

```Rust
extern crate reqwest;

use reqwest::header::AUTHORIZATION;
```

接着，您可以使用reqwest的get方法来发送一个基本认证的GET请求，并在headers中指定用户名和密码。

```Rust
let response = reqwest::get("http://example.com")?
    .header(AUTHORIZATION, "Basic <base64 encoded username:password>")
    .send()?;
```

如果认证成功，您将收到一个带有200状态码的响应，并可以通过response.text()方法来获取响应的内容。

```Rust
println!("Response body: {}", response.text()?);
```

## 深入了解通过基本认证发送HTTP请求

当您发送基本认证的HTTP请求时，您需要将用户名和密码以特定的方式编码，并将其放在headers中。基本认证使用基于Base64的编码方式，您可以使用Rust的base64库来实现编码。

```Rust
use base64::{encode};

let username = "example";
let password = "password";

let encoded_auth = encode(format!("{}:{}", username, password));
let auth_header = format!("Basic {}", encoded_auth);

let response = reqwest::get("http://example.com")?
    .header(AUTHORIZATION, auth_header)
    .send()?;
```

另外，如果您需要发送POST请求而不是GET请求，您也可以使用reqwest的post方法，并在body中指定要发送的数据。

```Rust
let body = "param1=value1&param2=value2";
let response = reqwest::post("http://example.com")?
    .header(AUTHORIZATION, auth_header)
    .body(body)
    .send()?;
```

您还可以使用reqwest的其他方法来细化请求，例如设置超时时间、自定义headers等。通过深入了解reqwest库的文档，您可以更深入地学习如何发送基本认证的HTTP请求。

## 查看更多

- [reqwest文档](https://docs.rs/reqwest)
- [Rust base64库文档](https://docs.rs/base64)
- [HTTP基本认证介绍](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Authentication)