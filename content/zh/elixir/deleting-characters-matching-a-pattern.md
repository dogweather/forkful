---
title:                "匹配模式的字符删除"
html_title:           "Elixir: 匹配模式的字符删除"
simple_title:         "匹配模式的字符删除"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 什么和为什么？

删除匹配模式的字符是指从一个字符串中移除满足特定模式的所有字符。程序员进行这种操作的原因通常是为了清洁数据或者进行数据转换，以便更容易地处理信息。

## 如何：

```Elixir
# 使用正则表达式删除所有数字
String.replace("Hello123 World456", ~r/[0-9]/, "")

# 输出：Hello World
```

```Elixir
# 使用模式列表删除所有元音字母
String.replace("Hello World", ~w[a e i o u], "")

# 输出：Hll Wrld
```

## 深入分析：

在历史上，程序员通常使用字符串函数来准备数据进行处理。然而，随着正则表达式的普及，删除字符匹配模式也变得更加容易。另外，Elixir提供了各种函数和操作符来进行字符串处理，使得删除字符匹配模式变得更加方便。

## 查看这些资源：

- [Elixir字符串处理函数](https://hexdocs.pm/elixir/String.html)
- [正则表达式在Elixir中的使用教程](https://elixir-lang.org/getting-started/regex.html)