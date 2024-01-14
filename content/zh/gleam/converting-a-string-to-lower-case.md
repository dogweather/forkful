---
title:    "Gleam: 将字符串转换为小写"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

为什么：进行字符串转换为小写的原因是为了更方便地处理和比较字符串。
## 为什么
在编程中，我们经常需要对字符串进行操作和比较。但是，由于字符串的大小写可能会对比较结果造成影响，因此我们需要将字符串统一转换为同一种大小写格式。这就是为什么我们会经常使用字符串转换为小写的操作。

## 如何操作
在Gleam中，我们可以使用内置的```String.to_lower```函数来实现字符串转换为小写。让我们来看一个简单的示例：

```Gleam
let string = "Hello World"
let lower_string = String.to_lower(string)
```
运行以上代码，我们会得到一个新的字符串```lower_string```，其值为```"hello world"```。这样，我们就可以使用统一的小写格式来进行后续的操作和比较。

另外，```String.to_lower```函数也可以接受第二个参数```locale```来指定转换的语言环境。例如，我们可以使用```String.to_lower(string, "en-US")```来将字符串转换为英文的小写格式。

## 深入了解
在Gleam的标准库中，字符串类型是通过Unicode来表示的。因此，在字符串转换为小写的过程中，我们也要考虑Unicode字符的特殊情况。比如，英文字符和希腊字母的小写形式并不是简单的字母替换，而是需要通过特定的规则进行转换。

除了```String.to_lower```函数外，Gleam还提供了```String.to_upper```函数来进行字符串转换为大写的操作。同样，我们也可以使用```String.to_title```函数来将字符串的首字母转换为大写。

## 参考链接
- [Gleam官方文档](https://gleam.run/documentation/)
- [Unicode中文网](https://www.unicode.org/charts/)
- [Gleam社区论坛](https://elixirforum.com/c/gleam/)

# 参见
- [Gleam总览：了解这门新兴函数式编程语言](https://example.com/article1)
- [使用Gleam构建并发应用程序的方法](https://example.com/article2)