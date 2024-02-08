---
title:                "解析HTML"
date:                  2024-02-03T19:11:59.384677-07:00
model:                 gpt-4-0125-preview
simple_title:         "解析HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
