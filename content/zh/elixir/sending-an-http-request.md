---
title:                "发出 HTTP 请求"
date:                  2024-01-20T17:59:21.561023-07:00
model:                 gpt-4-1106-preview
simple_title:         "发出 HTTP 请求"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (是什么&为什么?)
HTTP请求，就是从你的应用发向服务器的消息。程序员用它获取数据、提交表单、跟其他服务交互。

## How to: (如何做：)
Elixir中发送HTTP请求要用到外部库，比如`HTTPoison`。安装后，就这么写：

```elixir
# mix.exs 文件
defp deps do
  [{:httpoison, "~> 1.8"}]
end

# 然后在代码里：
HTTPoison.start()

case HTTPoison.get("https://api.example.com/data") do
  {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
    IO.puts("成功获取数据： #{body}")
  {:error, %HTTPoison.Error{reason: reason}} ->
    IO.puts("请求失败： #{reason}")
end
```

运行代码，会看到请求结果。

## Deep Dive (深入研究)
HTTP协议从1991年就被使用了，是互联网的基石。HTTPoison是Elixir用的库，基于Erlang的`hackney`。替代方案有`Tesla`、`HTTPotion`等。HTTPoison的好处是直接、易用，支持异步请求，但要处理错误，还得自己写点代码。

## See Also (另请参阅)
- HTTPoison文档: https://hexdocs.pm/httpoison
- HTTP协议细节: https://www.w3.org/Protocols/
- Tesla，一个Elixir的HTTP客户端库: https://github.com/teamon/tesla
