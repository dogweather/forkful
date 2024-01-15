---
title:                "将字符串转换为小写"
html_title:           "Elixir: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么

将字符串转换为小写是在Elixir编程中常见的操作，它可以很容易地转换包含大写和小写字母的字符串，从而使其在比较和匹配时更容易。它还可以帮助保持代码的统一性和可读性。

## 如何操作

```
Elixir
# 普通字符串
string = "Hello World"

# 使用String.downcase函数转换为小写
lowercase_string = String.downcase(string)

# 输出: "hello world"
IO.puts(lowercase_string)
```

```
Elixir
# 含有大写字母的字符串
string = "Hello World"

# 使用String.downcase函数转换为小写
lowercase_string = String.downcase(string)

# 输出: "hello world"
IO.puts(lowercase_string)

```

## 深入探究

使用String.downcase函数时，Elixir会在后台使用Unicode实现，这意味着它可以正确处理包含多字节字符的字符串。它还会处理非英文字符，例如德语的Umlauts或希腊语字母。

另外，如果你想要将字符串中的某些特定字母转换为小写，你可以使用String.replace函数来替换这些字母。

```
Elixir
# 含有特定字母的字符串
string = "HeLlO wOrLd"

# 将所有大写字母'o'转换为小写
lowercase_string = String.replace(string, "o", "O")

# 输出："HeLLo wOrLd"
IO.puts(lowercase_string)
```

## 参考

- [Elixir String Module](https://hexdocs.pm/elixir/String.html)
- [Elixir Unicode](https://hexdocs.pm/elixir/Unicode.Html)
- [Elixir String.downcase function](https://hexdocs.pm/elixir/String.html#downcase/1)
- [Elixir String.replace function](https://hexdocs.pm/elixir/String.html#replace/4)

## 参考
- [ Elixir String模块](https://hexdocs.pm/elixir/String.html)
- [ Elixir Unicode](https://hexdocs.pm/elixir/Unicode.Html)
- [ Elixir String.downcase函数](https://hexdocs.pm/elixir/String.html#downcase/1)
- [ Elixir String.replace函数](https://hexdocs.pm/elixir/String.html#replace/4)