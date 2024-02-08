---
title:                "下载网页"
aliases:
- zh/elixir/downloading-a-web-page.md
date:                  2024-01-20T17:44:02.359647-07:00
model:                 gpt-4-1106-preview
simple_title:         "下载网页"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/downloading-a-web-page.md"
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
