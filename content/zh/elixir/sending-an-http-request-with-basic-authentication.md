---
title:                "用基本认证发送HTTP请求"
html_title:           "Elixir: 用基本认证发送HTTP请求"
simple_title:         "用基本认证发送HTTP请求"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 做什么？为什么？

发送带有基本认证的HTTP请求是通过在请求头中包含用户名和密码来验证用户身份的一种方式。程序员通常这样做是因为可以确保请求只能被授权的用户访问，从而保证系统的安全性。

## 如何：

```Elixir
defmodule HTTP do
    @moduledoc """
    This module contains functions for sending HTTP requests with basic authentication.
    """
    
    @spec send_request(String.t, Keyword.t) :: HTTPoison.Response.t
    def send_request(url, options) do
        HTTPoison.get(url, options)
    end
end

response = HTTP.send_request("www.example.com", [basic_auth: {"username", "password"}])

IO.inspect response
```

输出：

```
%HTTPoison.Response{
  status_code: 200,
  body: "Hello world!",
  headers: [...]
}
```

## 深入了解：

基本认证是HTTP协议的一部分，它提供了一个简单的身份验证机制，可以确保请求只被授权的用户访问。除了基本认证，还有其他的身份验证方法，例如OAuth和JWT。程序员可以根据具体的需求选择合适的身份验证方法。

发出带有基本认证的HTTP请求，实际上就是在请求头中加入一个Authorization字段，其值格式为“Basic username:password”的Base64编码。服务器会根据这个认证信息来验证用户的身份真实性。

## 参考链接：

- [HTTP Basic authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [HTTPoison](https://github.com/edgurgel/httpoison) - Elixir的一个HTTP客户端库，支持基本认证。