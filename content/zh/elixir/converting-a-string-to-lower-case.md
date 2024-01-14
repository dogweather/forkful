---
title:    "Elixir: 将字符串转换为小写"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 为什么要将字符串转换为小写

在Elixir编程时，有时候我们需要将字符串中的所有字符转换为小写。这可能是因为我们需要对字符串进行比较或者输出时需要格式化。无论什么原因，将字符串转换为小写是一个常见的编程需求。

## 如何实现

在Elixir中，我们可以通过使用`String.downcase/2`函数来将字符串转换为小写。该函数接受两个参数，第一个参数为要转换的字符串，第二个参数为指定的字符编码。请注意，如果没有第二个参数，则默认使用UTF-8编码。

```Elixir
# 将字符串转换为小写，使用UTF-8编码
String.downcase("HELLO WORLD")
# 输出: "hello world"

# 将字符串转换为小写，使用ASCII编码
String.downcase("HELLO WORLD", "ASCII")
# 输出: "hello world"
```

在上面的代码中，我们分别将字符串"HELLO WORLD"转换为小写，使用了默认的UTF-8编码和指定的ASCII编码。无论使用哪种编码，最终输出的结果都是"hello world"。

## 深入了解

在Elixir中，字符串是不可变的。这意味着，当我们使用`String.downcase/2`函数来转换字符串为小写时，实际上该函数会创建一个新的字符串并返回，原始字符串并不会被修改。

此外，`String.downcase/2`函数的实现是基于Unicode字符的，因此它可以正确地处理各种语言的字符。这也意味着，即使字符串中包含特殊字符，函数也可以正确地转换为小写。

## 参考链接

- [Elixir String.downcase/2文档](https://hexdocs.pm/elixir/String.html#downcase/2)
- [Unicode编码简介](https://baike.baidu.com/item/Unicode/750500?fr=aladdin)
- [ASCII编码简介](https://baike.baidu.com/item/ASCII/309296?fr=aladdin)

# 参考链接