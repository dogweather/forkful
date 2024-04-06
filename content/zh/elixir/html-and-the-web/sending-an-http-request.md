---
date: 2024-01-20 17:59:21.561023-07:00
description: "How to: (\u5982\u4F55\u505A\uFF1A) Elixir\u4E2D\u53D1\u9001HTTP\u8BF7\
  \u6C42\u8981\u7528\u5230\u5916\u90E8\u5E93\uFF0C\u6BD4\u5982`HTTPoison`\u3002\u5B89\
  \u88C5\u540E\uFF0C\u5C31\u8FD9\u4E48\u5199\uFF1A."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:47.696814-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u505A\uFF1A) Elixir\u4E2D\u53D1\u9001HTTP\u8BF7\u6C42\u8981\
  \u7528\u5230\u5916\u90E8\u5E93\uFF0C\u6BD4\u5982`HTTPoison`\u3002\u5B89\u88C5\u540E\
  \uFF0C\u5C31\u8FD9\u4E48\u5199\uFF1A."
title: "\u53D1\u51FA HTTP \u8BF7\u6C42"
weight: 44
---

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
