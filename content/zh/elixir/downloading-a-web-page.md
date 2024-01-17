---
title:                "从网页上下载 (Cóng wǎngyè shàng xiàzài)"
html_title:           "Elixir: 从网页上下载 (Cóng wǎngyè shàng xiàzài)"
simple_title:         "从网页上下载 (Cóng wǎngyè shàng xiàzài)"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

当我们浏览网页时，实际上是在下载网页内容并显示在我们的屏幕上。程序员们也会使用类似的方式来下载网页内容，以便进一步处理和操作。

### 什么是下载网页和为什么程序员会这么做？

下载网页可以被视为一个程序员从网络上获取数据的方式。程序员们需要下载网页内容来进行数据分析、网页抓取和其他数据处理操作。这么做的目的是为了从网页中提取重要信息，或者将网页内容作为输入来进行其他操作。

### 如何实施：

```Elixir
require HTTPoison

{:ok, result} = HTTPoison.get("https://www.example.com")
```

上面的代码使用Elixir内置的HTTPoison库来获取网页内容，并将结果保存在`result`变量中。你可以在这之后使用`result`变量来进一步处理网页内容。

### 深入了解：

历史背景：在过去，下载网页是通过发送HTTP请求和接收HTTP响应来实现的。然而，随着网络技术的发展，现在可以使用更高效的技术来实现网页下载，例如WebSockets或gRPC。

替代方案：除了使用Elixir内置的HTTPoison库，程序员也可以使用其他下载网页的工具和库，例如Scrappy和Crawlkit。

实现详情：HTTPoison库使用Erlang的HTTP-client库来进行网页下载。HTTP-client库又调用Erlang的Inets库来进行底层的网络通信。

### 参考资料：

- [HTTPoison官方文档](https://hexdocs.pm/httpoison)
- [Scrappy库官方网站](https://github.com/elixir-scraper/scrappy)
- [Crawlkit库官方文档](https://hexdocs.pm/crawlkit)