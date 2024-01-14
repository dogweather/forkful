---
title:                "Gleam: 提取子字符串"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么要提取子字符串？

在编程中，有时我们需要从一个字符串中提取出一部分内容，这就是提取子字符串的作用。可能是为了进行某种操作，或者是为了提取出特定的信息。无论是什么原因，提取子字符串都是一项常用且有用的技能。

## 如何提取子字符串

提取子字符串的方法很简单，我们只需要使用Gleam中的`substring`函数即可。这个函数接受两个参数：要提取的原始字符串以及想要提取的部分的起始和结束位置。下面是一个例子：

```Gleam
let original_string = "Hello, world!"
let substring = substring(original_string, 7, 11)

io.print(substring)
```

这段代码的输出结果应该是`world`。我们可以看到，起始和结束位置分别是第七个和第十一个字符，而最终提取出的子字符串正是我们想要的部分。

## 深入了解提取子字符串

除了起始和结束位置，我们还可以使用`substring`函数的第三个参数指定一个步长。这样可以提取出间隔一定距离的字符。如果我们将第三个参数设置为2，则会提取出奇数位置的字符，而如果设置为3，则会提取出能被3整除的位置的字符。这些功能都使得提取子字符串更加灵活和实用。

## 参考链接

- [Gleam文档](https://gleam.run/documentation/)
- [Gleam](https://github.com/gleam-lang/gleam)
- [Gleam社区论坛](https://gleam-lang.discourse.group/)