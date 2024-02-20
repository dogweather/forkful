---
date: 2024-01-20 17:59:21.561023-07:00
description: "HTTP\u8BF7\u6C42\uFF0C\u5C31\u662F\u4ECE\u4F60\u7684\u5E94\u7528\u53D1\
  \u5411\u670D\u52A1\u5668\u7684\u6D88\u606F\u3002\u7A0B\u5E8F\u5458\u7528\u5B83\u83B7\
  \u53D6\u6570\u636E\u3001\u63D0\u4EA4\u8868\u5355\u3001\u8DDF\u5176\u4ED6\u670D\u52A1\
  \u4EA4\u4E92\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:06.433175
model: gpt-4-1106-preview
summary: "HTTP\u8BF7\u6C42\uFF0C\u5C31\u662F\u4ECE\u4F60\u7684\u5E94\u7528\u53D1\u5411\
  \u670D\u52A1\u5668\u7684\u6D88\u606F\u3002\u7A0B\u5E8F\u5458\u7528\u5B83\u83B7\u53D6\
  \u6570\u636E\u3001\u63D0\u4EA4\u8868\u5355\u3001\u8DDF\u5176\u4ED6\u670D\u52A1\u4EA4\
  \u4E92\u3002"
title: "\u53D1\u51FA HTTP \u8BF7\u6C42"
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
