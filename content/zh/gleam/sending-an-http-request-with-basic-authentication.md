---
title:                "Gleam: 以基本认证发送http请求"
simple_title:         "以基本认证发送http请求"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# 为什么

如果你想要与网络上的其他应用程序进行交互，你可能需要发送HTTP请求。基本认证是一种简单的身份验证方法，可以帮助你通过网络认证你的用户身份。因此，当你需要与另一个应用程序进行交互时，发送带有基本认证的HTTP请求是非常有用的。

# 如何

如果你正在使用Gleam编程语言，发送带有基本认证的HTTP请求是非常简单的。你只需要在你的代码中指定认证头，并使用一个可选的基本认证模块来生成认证字符串。下面是发送带有基本认证的HTTP请求的示例代码：

```
Gleam.http.send(
  method: "GET",
  url: "https://example.com/api/users",
  headers: [ ("Authorization", basic_auth("username", "password")) ]
)
```

这个示例代码将向`https://example.com/api/users`发送一个GET请求，并在认证头中包含基本认证字符串，以验证用户的身份。如果请求成功，它将返回一个包含用户列表的JSON响应。

# 深入探讨

如果你想了解更多关于发送带有基本认证的HTTP请求的信息，你可以查看[Gleam官方文档](https://gleam.run/articles/http-request.html#basic-authentication)。在那里，你可以找到更详细的示例和说明，以及如何处理错误和超时等问题。

# 参考链接

- Gleam官方文档：https://gleam.run/articles/http-request.html#basic-authentication
- HTTP基本认证解释：https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Authentication
- Gleam HTTP模块文档：https://gleam.run/modules/http.html