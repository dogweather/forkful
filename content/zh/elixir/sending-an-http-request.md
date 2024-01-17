---
title:                "发送一个http请求"
html_title:           "Elixir: 发送一个http请求"
simple_title:         "发送一个http请求"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

# 什么 & 为什么？
发送HTTP请求指的是向网络中的服务器发送一个请求，以获取所需的信息并接收相应的响应。程序员们常常会发送HTTP请求，因为它是构建网页和应用程序的重要组成部分。

# 如何进行：
这里有一些例子来演示如何用Elixir发送HTTP请求：

```
# 使用HTTPoison库来发送HTTP GET请求
HTTPoison.get!("https://example.com")

# 发送带有请求体和请求头的POST请求
HTTPoison.post!("https://example.com", %{body: "这是一个请求体"}, headers: [{"Content-Type", "application/json"}])
```

```
# 执行以上HTTP GET请求会得到以下输出：
{:ok, %HTTPoison.Response{status_code: 200, body: "<<响应体>>"}}
```

# 深入探讨：
发送HTTP请求的概念始于1990年代初期，是一种用来获取远程资源的标准化方式。除了使用HTTPoison库之外，Elixir还提供了其他的HTTP请求库，如: ibrowse和hackney。

# 参考链接：
- [Elixir官方文档：HTTPoison库](https://hexdocs.pm/httpoison/)
- [Elixir官方文档：发送HTTP请求](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html#httprb)
- [Elixir官方文档：其他的HTTP请求库](https://hexdocs.pm/elixir/HTTPoison.html#module-summary)