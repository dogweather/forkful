---
title:                "Elixir: 以基本身份验证发送http请求"
simple_title:         "以基本身份验证发送http请求"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# 为什么要使用HTTP请求和基本身份验证？

发送HTTP请求的主要目的是与服务器进行通信并获取所需的数据。使用基本身份验证可以确保在与服务器通信时安全，并且只有经过身份验证的用户才能访问受保护的数据。

# 如何实现使用Elixir发送具有基本身份验证的HTTP请求？

```elixir
# 引入HTTP请求库
require HTTPotion

# 创建HTTP请求
req = HTTPotion.get("https://www.example.com/api", [basic_auth: {"username", "password"}])

# 获取HTTP请求的状态码和响应体
status = req.status_code
body = req.body

# 打印输出状态码和响应体
IO.puts "Status Code: #{status}"
IO.puts "Response Body: #{body}"

```

# 深入了解基本身份验证的HTTP请求

基本身份验证是一种简单但有效的身份验证方法，它要求用户在发送HTTP请求时提供用户名和密码。使用基本身份验证时，这些信息将以Base64编码的格式发送给服务器，因此它并不是一种安全的身份验证方法。建议在与敏感数据交互时使用更安全的身份验证方法，如OAuth。

## 进一步学习

- [Elixir文档](https://elixir-lang.org/docs.html)
- [HTTPotion文档](https://hexdocs.pm/httpotion/readme.html)
- [基本身份验证(MD5加密)的HTTP请求](https://gist.github.com/MattSurabian/4633148)
- [OAuth身份验证](https://oauth.net/2/)