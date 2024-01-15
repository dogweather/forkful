---
title:                "使用基本身份验证发送http请求"
html_title:           "Elixir: 使用基本身份验证发送http请求"
simple_title:         "使用基本身份验证发送http请求"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# 为什么

基本认证是一种常见的身份验证方式，它可以用于发送HTTP请求以验证用户身份。对于想要在Elixir中使用HTTP请求的开发人员来说，基本认证是必不可少的。它可以帮助他们向特定的API或服务器发送请求，并获得授权访问。

# 如何做

通过Elixir的HTTPoison库发送HTTP请求是非常简单的。下面是一个示例代码，展示如何使用基本认证发送HTTP请求并获取响应：

```Elixir
response = HTTPoison.get("https://example.com/api", 
  headers: %{Authorization: "Basic base64(username:password)"} 
)
```

首先，我们使用`HTTPoison`模块的`get`函数来发送GET请求。第一个参数是我们想要发送请求的URL，第二个参数是一个map，其中包含我们想要在请求中包含的任何头部信息。在这里，我们使用`Authorization`头部来指定基本认证，并传递经过base64编码的用户名和密码。

如果请求成功，`response`变量将包含响应的结果，您可以通过使用`response.body`来获取响应体。

# 深入了解

为了理解HTTP基本认证的工作原理，我们可以先了解它的基本结构。基本认证通过在HTTP请求的`Authorization`头部中发送Base64编码的用户名和密码来验证用户身份。服务器接收到请求后，它会解码这些凭据并验证用户名和密码是否正确。如果是，服务器将允许客户端继续访问受保护的资源。

需要注意的是，基本认证并不是一种安全的认证方式，因为它只是简单地将凭据进行Base64编码而不是加密。因此，建议在生产环境中使用更安全的身份验证方式。

# 查看更多

想要了解更多关于Elixir以及HTTP基本认证的信息，请参阅以下资源：

- [Elixir官方文档](https://elixir-lang.org/docs.html)：可以在此找到Elixir的更多信息和文档。
- [HTTPoison文档](https://hexdocs.pm/httpoison/HTTPoison.html)：包含有关HTTPoison库的详细信息和示例。
- [HTTP基本认证文档](https://tools.ietf.org/html/rfc7617)：了解更多关于HTTP基本认证的细节和规范。