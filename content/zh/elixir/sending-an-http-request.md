---
title:                "发送http请求"
html_title:           "C#: 发送http请求"
simple_title:         "发送http请求"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 什么＆为什么？

HTTP请求的发送是在网络中从客户端到服务器传递信息的一种方式。程序员进行HTTP请求主要是为了从服务器获取数据，或者向服务器发送数据。

## 如何操作：

在 Elixir 中，发送 HTTP 请求可以使用包如 `HTTPoison`。首先，我们在 `mix.exs` 文件中加入依赖：

```Elixir
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end
```

然后用 `mix deps.get` 命令获取依赖。接着在代码中发送一个简单的 GET 请求：

```Elixir
HTTPoison.start

{status_code, response_body} = HTTPoison.get!("https://www.example.com").body
```

这将发送一个 GET 请求到 `www.example.com`，然后返回相应的状态码和响应体。

## 深度探讨：

HTTP请求由 HTTP 1.0 版本开始广泛应用，最初设计为满足分布式、协作、超媒体信息系统的需求。其他的方法发送 HTTP 请求包括使用 `Curl`，或者 `Postman`。

在Elixir中，当我们调用 `HTTPoison.get!` 时，实际上是使用了 Erlang 的 `:httpc` 模块。这个模块是Erlang标准库的一部分，专门用于处理 HTTP 协议。

Elixir的优势在于简洁易读的语法和优秀的并发性能，这使 HTTP 请求可以在充分利用服务器资源的同时保持代码的清晰和可维护性。

## 查阅相关信息：

1. Elixir官方文档: https://hexdocs.pm/elixir/
2. Elixir风格指南: https://github.com/elixir-lang/elixir
3. HTTPoison源码和文档: https://github.com/edgurgel/httpoison