---
title:                "将字符串转换为小写"
html_title:           "Gleam: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么
为什么我们要将一个字符串转换为小写呢？这是因为在程序中，我们经常需要比较两个字符串是否相同，而大小写是会影响到比较结果的。因此，通过将字符串转换为小写，可以保证比较的准确性。

## 如何实现
在Gleam中，实现字符串转换为小写非常简单。我们可以使用内置的`String.to_lower_case`函数来完成这个任务。下面是一个示例代码：

```
Gleam.code("let string = \"Hello World!\"
let lower_case = String.to_lower_case(string)
Gleam.debug(lower_case)")
```

运行这段代码后，你将会得到如下的输出：

```
"hello world!"
```

## 深入探究
在Gleam中，字符串被看作是一个由字符构成的列表。因此，通过遍历字符列表，将每个字符转换为小写，然后再组合成一个新的字符串，即可实现字符串转换为小写的功能。此外，Gleam还提供了`String.to_upper_case`函数来实现字符串转换为大写。

## 参考链接
- [Gleam文档](https://gleam.run/)
- [使用Gleam创建第一个Web应用](https://medium.com/swlh/build-your-first-web-app-with-gleam-79cb19d01e64)
- [Gleam代码示例集锦](https://github.com/gleam-examples)