---
date: 2024-01-20 17:44:02.359647-07:00
description: "\u4E0B\u8F7D\u7F51\u9875\u5C31\u662F\u4ECE\u4E92\u8054\u7F51\u4E0A\u83B7\
  \u53D6\u7F51\u9875\u7684\u5185\u5BB9\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u901A\
  \u5E38\u662F\u4E3A\u4E86\u6570\u636E\u6293\u53D6\u3001\u4FE1\u606F\u68C0\u7D22\u6216\
  \u5185\u5BB9\u76D1\u63A7\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:44.984958-07:00'
model: gpt-4-1106-preview
summary: "\u4E0B\u8F7D\u7F51\u9875\u5C31\u662F\u4ECE\u4E92\u8054\u7F51\u4E0A\u83B7\
  \u53D6\u7F51\u9875\u7684\u5185\u5BB9\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u901A\
  \u5E38\u662F\u4E3A\u4E86\u6570\u636E\u6293\u53D6\u3001\u4FE1\u606F\u68C0\u7D22\u6216\
  \u5185\u5BB9\u76D1\u63A7\u3002"
title: "\u4E0B\u8F7D\u7F51\u9875"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么?)
下载网页就是从互联网上获取网页的内容。程序员这么做通常是为了数据抓取、信息检索或内容监控。

## How to: (如何操作:)
```elixir
# 首先添加HTTPoison到mix.exs
defp deps do
  [{:httpoison, "~> 1.8"}]
end

# 然后运行 mix deps.get 来安装HTTPoison

# 现在使用HTTPoison获取网页内容
defmodule WebPageDownloader do
  def fetch(url) do
    case HTTPoison.get(url) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        {:ok, body}
      {:ok, %HTTPoison.Response{status_code: status_code}} ->
        {:error, "Error with status code: #{status_code}"}
      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, "Error due to: #{reason}"}
    end
  end
end

# 演示如何调用
WebPageDownloader.fetch("http://example.com")

# 假定输出示例:
{:ok, "<html>...</html>"}
```

## Deep Dive (深入探索)
早期下载网页主要用于搜索引擎和数据挖掘。Elixir通过库如 HTTPoison 和 Tesla 简化了网页下载，背后使用了 Erlang 的强大并发特性。相对于其它语言，Elixir 在处理成千上万并发连接时更为高效。虽然有其他方式（如使用 :httpc 模块），HTTPoison 在社区中更受欢迎，因为它提供了一套丰富而便捷的 HTTP 客户端 API。

## See Also (另请参阅)
- [HTTPoison GitHub repository](https://github.com/edgurgel/httpoison)
- [Erlang :httpc documentation](http://erlang.org/doc/man/httpc.html)
