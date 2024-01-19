---
title:                "下载网页"
html_title:           "Arduino: 下载网页"
simple_title:         "下载网页"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 什么以及为什么?
下载网页是获取服务器上某网页的所有数据并将其保存到本地的过程。程序员经常为了离线访问或进行数据抓取需要下载网页。

## 怎么做:
Elixir 的 `HTTPoison` 包可用于下载网页。执行以下步骤来进行:

1.  在项目中添加依赖项, 在 `mix.exs` 文件的 `deps` 函数中添加 `{:httpoison, "~> 1.8"}`.

```Elixir
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end
```

2. 下载并编译依赖项: `mix deps.get`.
3. 创建一个函数去发起 GET 请求并获取网页的body.

```Elixir
defmodule PageDownloader do
  def download(url) do
    case HTTPoison.get(url) do
      {:ok, %HTTPoison.Response{body: body}} -> 
        IO.puts body
      {:error, %HTTPoison.Error{reason: reason}} -> 
        IO.puts "Error: #{reason}"
    end
  end
end
```

运行此代码将获取网页的 body 并打印出来.

## 深度理解
在历史长河中，从 web 上下载内容一直是一个普遍的任务。起初，开发者需要与基础的 HTTP 协议进行交互，但现在已经有了众多的库负责处理。

Elixir 的其他包，比如 `Tesla` 和 `Gun`，也可以执行下载网页的任务。`HTTPoison` 相对于这些选择，它更简单且易于使用，特别是对于新手来说。但如果你有特殊需求，例如需要握手，可能需要使用这些其他选项。

具体实现详情，`HTTPoison.get(url)` 实际上是将 HTTP GET请求发送到指定的URL，并返回服务器的响应。响应是 `{:ok, response}` 或 `{:error, reason}`：前者包含响应体（即网页内容），后者则包含错误原因。

## 参考来源
- HTTPoison 文档: https://hexdocs.pm/httpoison/readme.html
- Elixir 语言官方网站: https://elixir-lang.org/
- Elixir 的其他 HTTP 客户端: https://hexdocs.pm/awesome-elixir/#http-client