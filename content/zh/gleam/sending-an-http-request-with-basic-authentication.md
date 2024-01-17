---
title:                "使用基本认证发送http请求"
html_title:           "Gleam: 使用基本认证发送http请求"
simple_title:         "使用基本认证发送http请求"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
发送HTTP请求时使用基本认证是通过在请求头中包含用户名和密码来验证用户身份的一种方式。程序员这样做的目的是为了保护敏感信息，例如登录凭证或个人数据，防止未经授权的访问。

## 如何:
使用Gleam编程示例和```Gleam ... ```代码块展示了如何发送带有基本认证的HTTP请求，并展示了相应的输出结果。

```Gleam
// 导入http模块
import gleam/http

// 创建基本认证的用户名和密码
let username = "john"
let password = "secret"

// 创建HTTP认证头
let auth = http.basic_auth(username, password)

// 发送GET请求并打印结果
http.get("http://www.example.com/", headers=[auth]) |> Ok
    |> print
```

输出: `Ok({MyWebPage})`

## 深入探究:
历史背景: 基本认证最早出现在HTTP/1.0协议中，后来在HTTP/1.1中得到了通用的支持。

替代方案: 除了基本认证外，还有其他多种认证方式，如摘要认证、OAuth等。

实现细节: 基本认证使用Base64编码来加密用户名和密码，在网络中仍然是以明文传输，存在被破解的风险，因此建议结合使用HTTPS来增加安全性。

## 参考资料:
了解更多关于基本认证的信息，请参考以下链接:
- [HTTP Authentication: Basic and Digest Access Authentication](https://tools.ietf.org/html/rfc2617)
- [HTTP Basic authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)