---
title:                "Gleam: 计算字符串的长度"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

为什么：只需1-2句话就可以解释*为什么*会有人参与查找字符串的长度。

字怎么写：在“```Gleam ... ```”代码块内编写示例代码和样本输出。

深入研究：关于查找字符串长度的更深层信息。

查看：Markdown标题为“另请参阅”（翻译为中文），后跟链接列表。

## 为什么

寻找字符串长度可能是编程中经常遇到的任务之一。无论是验证用户输入的正确性，还是处理大量文本数据，都需要知道字符串的长度。因此，掌握如何找到字符串长度是必不可少的技能。

## 怎么写

要编写代码来找到字符串的长度，我们可以使用Gleam编程语言提供的内置函数。例如，我们可以使用“str.len”来查找字符串的长度，并将其存储在变量中，然后输出这个变量，如下所示：

```
Gleam

let string = "Hello, world!"
let length = str.len(string)
// 输出：13
```

我们也可以将字符串长度的结果直接打印出来，而无需使用额外的变量。以下是一个完整的示例代码：

```
Gleam

let string = "This is a longer string."
io.print("The length of the string is ", str.len(string))
// 输出：The length of the string is 25
```

## 深入研究

要理解找到字符串长度的原理，我们需要了解字符串是如何在计算机内存中存储的。每个字符都以数字的形式被存储，这个数字对应着字符的ASCII码。因此，我们可以通过计算出字符串中字符的数量来找到字符串的长度。

另外，还需要注意的一点是，当我们在编写代码中使用字符串时，它们实际上是一个数组，因为它们由一系列的字符组成。因此，我们也可以使用数组的方法来找到字符串的长度。

## 另请参阅

- [Gleam官方网站](https://gleam.run/)
- [Gleam中文文档](https://gleam.run/zh-cn/docs/)