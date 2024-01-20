---
title:                "使用基本认证发送http请求"
html_title:           "Bash: 使用基本认证发送http请求"
simple_title:         "使用基本认证发送http请求"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 什么与为什么？

HTTP基本认证是一种允许用户提供用户名和密码的方式来发送HTTP请求。程序员之所以使用它，是因为它为服务器端验证客户端身份提供了一种简单且快速的方法。

## 如何操作：

在Elixir中，我们可以使用HTTPoison库发送带有基本认证的HTTP请求。以下是一个示例:

```Elixir 
{:ok, response} = 
    HTTPoison.get("https://api.github.com", [], 
    basic_auth: {"username", "password"})
IO.inspect response.status_code
```

执行上述代码后，我们可以在终端输出中看到状态码。

```Elixir 
200
```

此代码示例发送了一个GET请求到Github的API，并以用户名和密码对作为基本认证。

## 深入了解

HTTP基本认证可以追溯到HTTP/1.0规范，旨在为应用程序提供一种简单验证方式。尽管如此，由于发送的凭据未经加密，因此不应在没有HTTPS的情况下使用。

在Elixir中，除HTTPoison外，我们还有其他选择进行HTTP请求，如Mint和Tesla。这些库提供了不同的API和不同的特性，你可以根据你的需求来选择。

实现基本认证的关键在于在请求的头部添加一个`Authorization`字段，值为"Basic "后跟由用户名和密码构成的经过Base64编码的字符串。

## 另请参阅：

- [Elixir Docs: HTTPoison](https://hexdocs.pm/httpoison/readme.html)
- [HTTP Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)