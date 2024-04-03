---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:59.384677-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Elixir \u51ED\u501F\u5176\u5065\u58EE\
  \u7684\u5E76\u53D1\u6A21\u578B\u548C\u51FD\u6570\u5F0F\u7F16\u7A0B\u8303\u4F8B\uFF0C\
  \u5E76\u4E0D\u5305\u542B\u5185\u7F6E\u7684 HTML \u89E3\u6790\u529F\u80FD\u3002\u7136\
  \u800C\uFF0C\u60A8\u53EF\u4EE5\u4F7F\u7528\u50CF `Floki` \u8FD9\u6837\u7684\u6D41\
  \u884C\u7B2C\u4E09\u65B9\u5E93\u6765\u5B9E\u73B0\u6B64\u76EE\u7684\u3002Floki \u4F7F\
  \u5F97 HTML \u89E3\u6790\u76F4\u89C2\u4E14\u9AD8\u6548\uFF0C\u5229\u7528 Elixir\
  \ \u7684\u6A21\u5F0F\u5339\u914D\u548C\u7BA1\u9053\u7279\u6027\u3002 \u9996\u5148\
  \uFF0C\u5C06 Floki \u6DFB\u52A0\u5230\u60A8\u7684 mix.exs\u2026"
lastmod: '2024-03-13T22:44:47.357074-06:00'
model: gpt-4-0125-preview
summary: "Elixir \u51ED\u501F\u5176\u5065\u58EE\u7684\u5E76\u53D1\u6A21\u578B\u548C\
  \u51FD\u6570\u5F0F\u7F16\u7A0B\u8303\u4F8B\uFF0C\u5E76\u4E0D\u5305\u542B\u5185\u7F6E\
  \u7684 HTML \u89E3\u6790\u529F\u80FD\u3002\u7136\u800C\uFF0C\u60A8\u53EF\u4EE5\u4F7F\
  \u7528\u50CF `Floki` \u8FD9\u6837\u7684\u6D41\u884C\u7B2C\u4E09\u65B9\u5E93\u6765\
  \u5B9E\u73B0\u6B64\u76EE\u7684\u3002Floki \u4F7F\u5F97 HTML \u89E3\u6790\u76F4\u89C2\
  \u4E14\u9AD8\u6548\uFF0C\u5229\u7528 Elixir \u7684\u6A21\u5F0F\u5339\u914D\u548C\
  \u7BA1\u9053\u7279\u6027."
title: "\u89E3\u6790HTML"
weight: 43
---

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
