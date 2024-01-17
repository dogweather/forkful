---
title:                "查找字符串的长度"
html_title:           "Elixir: 查找字符串的长度"
simple_title:         "查找字符串的长度"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

什么是字符串长度？
字符串长度是指一个字符串中包含的字符的个数。在编程中，字符串长度通常用来计算一个字符串的大小，并且经常作为程序的重要参数使用。

为什么程序员要计算字符串长度？
在编程中，我们经常需要处理文本和字符串数据。为了正确地处理和操作这些数据，我们需要知道它们的长度。比如，当我们需要限制用户输入的字符串长度时，就需要计算字符串长度来确保它不会超出指定的范围。

如何计算字符串长度？
在Elixir中，我们可以使用内建函数 `String.length` 来计算字符串的长度，它会返回字符串中字符的个数。下面是一个简单的例子：

```elixir
sentence = "Hello, world!"
String.length(sentence)
```

这段代码将返回 `13`，因为这个字符串由13个字符组成。

深入了解
在历史上，计算字符串长度是一个非常耗时的操作，因为它需要遍历整个字符串来计算它的长度。但是在现代编程语言中，这个操作已经被高效地实现，所以我们可以在实际应用中使用它。

除了使用 `String.length` 函数，我们也可以使用模式匹配来计算字符串的长度。比如，下面的代码会将字符串中的每个字符绑定到一个变量，并返回它们的长度：

```elixir
sentence = "Hello, world!"
length = len(sentence)

def len(""), do: 0
def len(string), do: len(tl(string)) + 1
```

在这个例子中，我们利用了模式匹配的能力来递归地计算字符串的长度。这种方法在一些特定的场景下可能比使用 `String.length` 函数更快。

此外，如果我们需要计算一个字符串的字节数（而不是字符数），我们可以使用 `String.length` 函数的另一个变体 `String.length_codepoints`。

相关链接
- [Elixir文档：String.length](https://hexdocs.pm/elixir/String.html#length/1)
- [Elixir源码：string.ex](https://github.com/elixir-lang/elixir/blob/v1.10.0/lib/elixir/lib/string.ex)