---
title:    "Elixir: 删除匹配模式的字符"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## 为什么要删除匹配模式的字符？

有时候在编程中，我们会遇到需要删除特定字符的情况。这可能是为了清洗数据，提取特定内容等等。使用Elixir编程语言，我们可以很容易地根据特定模式删除字符，让我们来学习如何做到这一点！

## 如何实现

要删除匹配模式的字符，我们可以使用Elixir内置的`String.replace/3`函数。它接受三个参数：源字符串、需要替换的模式和替换后的字符串。

```Elixir
# 要删除所有数字
input = "abc123def456ghi"

String.replace(input, ~r/[0-9]/, "") # 输出 "abcdefghi"
```

在这个例子中，我们使用了正则表达式模式`[0-9]`来匹配所有数字，并将它们替换为空字符串，从而删除了所有数字。

我们也可以使用`String.replace/3`函数来删除特定单词：

```Elixir
# 要删除"Elixir"这个单词
input = "I love Elixir, it's my favorite language!"

String.replace(input, "Elixir", "") # 输出 "I love , it's my favorite language!"
```

在这个例子中，我们将"Elixir"替换为空字符串，从而删除了这个单词。

## 深入探讨

除了使用简单的字符串和正则表达式来匹配字符，我们也可以使用更复杂的模式来删除字符。Elixir的`String.replace/3`函数可以接受任何一个实现了`String.Chars`协议的数据结构作为模式。

```Elixir
# 要删除所有小写字母
input = "Hello, world!"
pattern = String.Chars.downcase().(&(&1 in ?a..?z))

String.replace(input, pattern, "") # 输出 "H, !"
```

在这个例子中，我们使用了一个lambda表达式来定义模式，它将匹配所有小写字母并将它们替换为空字符串，从而删除了所有小写字母。

我们还可以使用函数作为模式来动态地选择需要删除的字符：

```Elixir
# 要删除所有偶数
input = [1, 2, 3, 4, 5, 6]
pattern = fn x -> rem(x, 2) == 0 end

Enum.filter(input, pattern) # 输出 [1, 3, 5]
```

在这个例子中，我们定义了一个函数模式，它判断一个数字是否为偶数，然后使用`Enum.filter/2`函数来过滤出所有奇数，并删除了所有偶数。

## 见证神奇的删除能力！

现在我们已经了解了如何使用`String.replace/3`函数来删除字符，是不是感觉很神奇？不仅仅是简单的字符串和正则表达式，我们还可以使用函数等更复杂的模式来实现更强大的删除能力。赶快去尝试一下吧！

## 看看这些有用的链接吧

- [Elixir官方文档 - String.replace/3](https://hexdocs.pm/elixir/String.html#replace/3)
- [正则表达式指南](https://regexone.com)
- [Elixir论坛](https://elixirforum.com)