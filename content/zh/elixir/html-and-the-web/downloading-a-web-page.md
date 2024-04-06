---
date: 2024-01-20 17:44:02.359647-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C:) \u65E9\u671F\u4E0B\u8F7D\u7F51\u9875\
  \u4E3B\u8981\u7528\u4E8E\u641C\u7D22\u5F15\u64CE\u548C\u6570\u636E\u6316\u6398\u3002\
  Elixir\u901A\u8FC7\u5E93\u5982 HTTPoison \u548C Tesla \u7B80\u5316\u4E86\u7F51\u9875\
  \u4E0B\u8F7D\uFF0C\u80CC\u540E\u4F7F\u7528\u4E86 Erlang \u7684\u5F3A\u5927\u5E76\
  \u53D1\u7279\u6027\u3002\u76F8\u5BF9\u4E8E\u5176\u5B83\u8BED\u8A00\uFF0CElixir \u5728\
  \u5904\u7406\u6210\u5343\u4E0A\u4E07\u5E76\u53D1\u8FDE\u63A5\u65F6\u66F4\u4E3A\u9AD8\
  \u6548\u3002\u867D\u7136\u6709\u5176\u4ED6\u65B9\u5F0F\uFF08\u5982\u4F7F\u7528 :httpc\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:47.699209-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C:) \u65E9\u671F\u4E0B\u8F7D\u7F51\u9875\u4E3B\u8981\
  \u7528\u4E8E\u641C\u7D22\u5F15\u64CE\u548C\u6570\u636E\u6316\u6398\u3002Elixir\u901A\
  \u8FC7\u5E93\u5982 HTTPoison \u548C Tesla \u7B80\u5316\u4E86\u7F51\u9875\u4E0B\u8F7D\
  \uFF0C\u80CC\u540E\u4F7F\u7528\u4E86 Erlang \u7684\u5F3A\u5927\u5E76\u53D1\u7279\
  \u6027\u3002\u76F8\u5BF9\u4E8E\u5176\u5B83\u8BED\u8A00\uFF0CElixir \u5728\u5904\u7406\
  \u6210\u5343\u4E0A\u4E07\u5E76\u53D1\u8FDE\u63A5\u65F6\u66F4\u4E3A\u9AD8\u6548\u3002\
  \u867D\u7136\u6709\u5176\u4ED6\u65B9\u5F0F\uFF08\u5982\u4F7F\u7528 :httpc \u6A21\
  \u5757\uFF09\uFF0CHTTPoison \u5728\u793E\u533A\u4E2D\u66F4\u53D7\u6B22\u8FCE\uFF0C\
  \u56E0\u4E3A\u5B83\u63D0\u4F9B\u4E86\u4E00\u5957\u4E30\u5BCC\u800C\u4FBF\u6377\u7684\
  \ HTTP \u5BA2\u6237\u7AEF API\u3002"
title: "\u4E0B\u8F7D\u7F51\u9875"
weight: 42
---

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
