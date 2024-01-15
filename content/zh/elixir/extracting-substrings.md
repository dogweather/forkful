---
title:                "截取子字符串"
html_title:           "Elixir: 截取子字符串"
simple_title:         "截取子字符串"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么要提取子字符串

提取子字符串是一种常见的文本处理操作，它可以帮助我们从一段文本中快速获取所需的信息。作为Elixir语言中的字符串处理功能之一，它可以让我们更便捷地处理各种任务，例如文本匹配、数据清洗等。

## 如何实现提取子字符串

在Elixir中，我们可以使用`String.slice`函数来提取子字符串。该函数接受三个参数：待提取字符串、起始位置和结束位置。其中起始位置和结束位置都是从0开始计数，表示从哪个字符开始提取和提取到哪个字符为止。让我们来看一个例子：

```Elixir
text = "Hello, world!"
String.slice(text, 7, 11)
```

运行以上代码会得到如下输出：

```
"world"
```

我们也可以使用负数作为结束位置来表示从字符串末尾开始提取，例如：

```Elixir
text = "Hello, world!"
String.slice(text, 7, -1)
```

这会提取出从第7个字符到字符串末尾的部分，输出为：

```
"world!"
```

## 深入了解提取子字符串

除了通过起始位置和结束位置来提取子字符串，我们还可以利用正则表达式来进行提取。Elixir提供了`Regex.run`函数来执行正则表达式匹配，然后再通过`String.slice`来提取所需的部分。例如：

```Elixir
text = "I have 3 apples and 5 bananas"
Regex.run(~r/([0-9]+) bananas/, text)
|> hd
|> elem(1)
|> String.to_integer()
```

以上代码会先匹配字符串中的`5 bananas`部分，然后通过`hd`函数取出匹配结果的第一个元素（即完整匹配的部分），再通过`elem(1)`来取出第二个捕获组（即只匹配数字部分），最后通过`String.to_integer()`将字符串转换成整数。

除了提取，Elixir还提供了`String.split`函数来拆分字符串，以及`String.replace`函数来替换字符串中的内容。这些功能都可以与提取子字符串一起配合使用，达到更复杂的文本处理目的。

## 参考链接

- [Elixir官方文档-String](https://hexdocs.pm/elixir/String.html)
- [Elixir官方文档-Regex](https://hexdocs.pm/elixir/Regex.html)
- [Elixir School-Strings](https://elixirschool.com/zh-cn/lessons/basics/built-in-types-strings/)
- [Elixir中文文档-字符串模式匹配](https://github.com/liuminous/docs-cn/blob/master/docs/core/string-pattern-matching.md)