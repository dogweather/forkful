---
date: 2024-01-26 03:38:40.842944-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Elixir\u6CA1\u6709\u5185\u5EFA\u7684\u201C\
  \u79FB\u9664\u5F15\u53F7\u201D\u51FD\u6570\uFF0C\u4F46\u5229\u7528\u6A21\u5F0F\u5339\
  \u914D\u6216`String`\u51FD\u6570\u81EA\u884C\u5B9E\u73B0\u5374\u5F88\u7B80\u5355\
  \u3002\u770B\u770B\u8FD9\u4E9B\u4EE3\u7801\u7247\u6BB5\uFF1A."
lastmod: '2024-04-05T22:38:46.521275-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A Elixir\u6CA1\u6709\u5185\u5EFA\u7684\u201C\
  \u79FB\u9664\u5F15\u53F7\u201D\u51FD\u6570\uFF0C\u4F46\u5229\u7528\u6A21\u5F0F\u5339\
  \u914D\u6216`String`\u51FD\u6570\u81EA\u884C\u5B9E\u73B0\u5374\u5F88\u7B80\u5355\
  \u3002\u770B\u770B\u8FD9\u4E9B\u4EE3\u7801\u7247\u6BB5\uFF1A."
title: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u79FB\u9664\u5F15\u53F7"
weight: 9
---

## 如何操作：
Elixir没有内建的“移除引号”函数，但利用模式匹配或`String`函数自行实现却很简单。看看这些代码片段：

```elixir
# 使用模式匹配
def unquote_string("\"" <> quoted_string <> "\""), do: quoted_string
def unquote_string("'" <> quoted_string <> "'"), do: quoted_string
def unquote_string(quoted_string), do: quoted_string

# 示例使用
unquote_string("\"你好，世界！\"") # => "你好，世界！"
unquote_string("'你好，世界！'")   # => "你好，世界！"

# 使用String.trim/1
def unquote_string(string), do: String.trim(string, "'\"")

# 示例使用
unquote_string("\"你好，世界！\"") # => "你好，世界！"
unquote_string("'你好，世界！'")   # => "你好，世界！"
```

两种方法的输出将是：
```
"你好，世界！"
```

## 深入了解
回到过去，字符串中的引号是一个雷区——处理不当，轰，语法错误或安全漏洞。在Elixir中，模式匹配像处理乐高积木一样处理你的字符串，让你可以准确地拆分和重建。它强大的`String`模块也很方便，灵活地使用`trim`函数去除引号。其他选择？正则表达式可以轻松处理引号，外部库如果你需要超出基本剥离的功能，可能会提供额外的火力。

## 另见
深入了解这些：
- [Elixir的String模块](https://hexdocs.pm/elixir/String.html)
- [了解更多关于Elixir中的模式匹配](https://elixir-lang.org/getting-started/pattern-matching.html)
- [Elixir中的正则表达式（Regex模块）](https://hexdocs.pm/elixir/Regex.html)
