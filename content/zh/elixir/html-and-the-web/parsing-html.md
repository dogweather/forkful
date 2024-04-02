---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:59.384677-07:00
description: "\u5728 Elixir \u4E2D\u89E3\u6790 HTML \u6D89\u53CA\u5230\u4ECE HTML\
  \ \u6587\u6863\u4E2D\u63D0\u53D6\u4FE1\u606F\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u662F\u4E3A\u4E86\u4EE5\u7F16\u7A0B\u65B9\u5F0F\u4E0E\u7F51\u9875\u4E92\u52A8\
  \u3001\u6293\u53D6\u6570\u636E\u6216\u81EA\u52A8\u5316\u7F51\u7EDC\u4EA4\u4E92\uFF0C\
  \u4F7F\u5E94\u7528\u7A0B\u5E8F\u80FD\u591F\u52A8\u6001\u7406\u89E3\u548C\u5229\u7528\
  \u7F51\u9875\u5185\u5BB9\u3002"
lastmod: '2024-03-13T22:44:47.357074-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Elixir \u4E2D\u89E3\u6790 HTML \u6D89\u53CA\u5230\u4ECE HTML \u6587\
  \u6863\u4E2D\u63D0\u53D6\u4FE1\u606F\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\
  \u4E3A\u4E86\u4EE5\u7F16\u7A0B\u65B9\u5F0F\u4E0E\u7F51\u9875\u4E92\u52A8\u3001\u6293\
  \u53D6\u6570\u636E\u6216\u81EA\u52A8\u5316\u7F51\u7EDC\u4EA4\u4E92\uFF0C\u4F7F\u5E94\
  \u7528\u7A0B\u5E8F\u80FD\u591F\u52A8\u6001\u7406\u89E3\u548C\u5229\u7528\u7F51\u9875\
  \u5185\u5BB9\u3002"
title: "\u89E3\u6790HTML"
weight: 43
---

## 什么 & 为什么？

在 Elixir 中解析 HTML 涉及到从 HTML 文档中提取信息。程序员这样做是为了以编程方式与网页互动、抓取数据或自动化网络交互，使应用程序能够动态理解和利用网页内容。

## 如何操作：

Elixir 凭借其健壮的并发模型和函数式编程范例，并不包含内置的 HTML 解析功能。然而，您可以使用像 `Floki` 这样的流行第三方库来实现此目的。Floki 使得 HTML 解析直观且高效，利用 Elixir 的模式匹配和管道特性。

首先，将 Floki 添加到您的 mix.exs 依赖中：

```elixir
defp deps do
  [
    {:floki, "~> 0.31.0"}
  ]
end
```

然后，运行 `mix deps.get` 以安装新的依赖项。

现在，让我们解析一个简单的 HTML 字符串以提取数据。我们将寻找 `<h1>` 标签内的标题：

```elixir
html_content = """
<html>
  <body>
    <h1>Hello, Elixir!</h1>
    <h1>Another Title</h1>
  </body>
</html>
"""

titles = html_content
         |> Floki.find("h1")
         |> Floki.text()

IO.inspect(titles)
```

**示例输出：**

```elixir
["Hello, Elixir!", "Another Title"]
```

要深入一步，假设您想提取链接（`<a>` 标签）及其 href 属性。以下是您可以实现的方式：

```elixir
html_content = """
<html>
  <body>
    <a href="https://elixir-lang.org/">Elixir 官方网站</a>
    <a href="https://hexdocs.pm/">HexDocs</a>
  </body>
</html>
"""

links = html_content
        |> Floki.find("a")
        |> Enum.map(fn({_, attrs, [text]}) -> {text, List.keyfind(attrs, "href", 0)} end)
        
IO.inspect(links)
```

**示例输出：**

```elixir
[{"Elixir 官方网站", {"href", "https://elixir-lang.org/"}}, {"HexDocs", {"href", "https://hexdocs.pm/"}}]
```

这种方法允许您高效地导航和解析 HTML 文档，使得在 Elixir 应用程序中进行网页数据提取和操作任务变得直接且简单。
