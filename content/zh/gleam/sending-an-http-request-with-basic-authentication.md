---
title:                "使用基本认证发送http请求"
html_title:           "Bash: 使用基本认证发送http请求"
simple_title:         "使用基本认证发送http请求"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 什么与为什么?

当我们发送带有基础认证的 HTTP 请求时，我们在 Web 服务请求中包含一组凭证。程序员这么做是为了通过 Web 服务器在无需交互的情况下验证用户的身份。

## 如何实现:

在Gleam中，我们可以使用`gleam/httpc`库发送HTTP请求。让我们看一个示例：

```Gleam
import gleam/httpc.{Get, send}
import gleam/httpc/header.{RequestHeaders, Authorization}

fn request_with_auth(url: String, username: String, password: String) {
  let auth_header = Authorization.basic(username, password)
  let headers = RequestHeaders.new()
    |> RequestHeaders.append(auth_header)
  send(Get(url), [headers])
}
```

当你运行这个代码片段时，你将能够发送一个带有基本授权的 HTTP GET 请求。

## 深入了解

发送带有基本认证的HTTP请求阐明了Web服务的安全性。虽然“基础”认证并不提供强大的安全性（因为它通过简单的 base64 编码发送用户凭证），但它在历史上是 Web 认证的重要部分，尤其是在 中间人你不需要或不能对请求进行加密的场景下。

对于发送HTTP请求有多种实现方式，如使用cookies或token，或者使用更复杂的认证方式如OAuth。

在代码实现细节中，我们是在HTTP请求头中包含了`Authorization`头来发送凭证。其中，`Authorization.basic`是`gleam/httpc`库中的一个函数，生成了一个基本认证头。

## 参考资料 

- Gleam HTTP 客户端文档: https://hexdocs.pm/gleam_httpc/readme.html
- HTTP 认证: https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Authentication