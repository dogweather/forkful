---
title:                "使用基本认证发送 HTTP 请求"
date:                  2024-01-20T18:01:34.372731-07:00
model:                 gpt-4-1106-preview
simple_title:         "使用基本认证发送 HTTP 请求"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
发送带有基本认证的HTTP请求就是通过网络向服务器发送信息，并携带用户名和密码进行验证。程序员这样做是为了安全地访问受保护的资源，比如API端点。

## 如何做:
```gleam
import gleam/http
import gleam/http/cowboy

// 定义认证凭证
fn basic_auth_header(username: String, password: String) -> http.Header {
  let credentials = base64.encode(username ++ ":" ++ password)
  http.header("Authorization", "Basic " ++ credentials)
}

// 发送请求
pub fn send_authenticated_request() {
  let auth_header = basic_auth_header("user", "pass")
  let request = http.Request(
    method: http.Get,
    headers: [auth_header],
    url: "http://example.com/protected",
    ..http.default_request()
  )

  let response = http.send(request)

  match response {
    Ok(response) -> io.println("Success! Response: " ++ response.body)
    Error(error) -> io.println("Error sending request: " ++ error)
  }
}
```
输出示例: 
```
Success! Response: Welcome to the protected endpoint!
```

## 深入了解
HTTP基本认证是一种简单的认证协议，客户端通过Authorization头部发送用户名和密码。这种方法存在于HTTP/1.0，至今仍被应用。尽管基本认证易于实现，但由于凭证以明文形式发送（尽管经过Base64编码），因此不安全——如果不是在HTTPS上使用。

替代方案包括OAuth和Token-based认证，它们都较为复杂但提供了更高的安全性。

在Gleam中发送带有基本认证的HTTP请求，首先需要构造一个带有`Authorization`头部的请求，然后使用`http.send`函数发送。Gleam的类型系统和模式匹配功能让处理响应简洁且安全。

## 参考链接
- Gleam HTTP客户端文档: [https://hexdocs.pm/gleam_http/](https://hexdocs.pm/gleam_http/)
- HTTP认证标准: [https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- Base64编码说明: [https://developer.mozilla.org/en-US/docs/Glossary/Base64](https://developer.mozilla.org/en-US/docs/Glossary/Base64)