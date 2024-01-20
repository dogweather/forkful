---
title:                "提取子字符串"
html_title:           "Arduino: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么是子字符串提取，为什么要用？
子字符串提取是从字符串中获取指定部分内容的操作。程序员们用它来分析和操作字符串数据。

## 如何使用：
```Gleam
let substring = string.slice(start: 3, length: 4)
let () = io.println(substring)  // 输出 "lo W"
```
我们创建了一个新字符串，其中包含从第三个字符开始的四个字符。

## 深入了解：
子字符串提取起源于计算机科学早期的编程语言。无论在古老的Fortran，还是在现代的Python，你总能看到它的身影。提取子字符串是操作字符串的基本方式之一。

用Gleam提取子字符串有很多方式，我们上面展示的是其中之一。另一种常见的做法是使用函数`string.split_at`，它会将字符串一分为二，然后返回两个新的字符串。

```Gleam
let (first_half, second_half) = string.split_at(index: 7)
let () = io.println(first_half) // 输出 "Hello, "
let () = io.println(second_half) // 输出 "World!"
```

这样我们就可以得到两个新的子字符串，每个都包含原字符串的一部分。

## 参考资料：
你可以查看[Tutorialspoint](https://www.tutorialspoint.com/programming_languages/gleam_string_types.htm)上关于Gleam String Types的更多信息。

Gleam的[官方文档](https://gleam.run/documentation/)是更深入了解字符串操作和其他Gleam功能的好地方。

你也可以在[Gleam Cookbook](https://github.com/gleam-lang/stdlib/tree/main/src/gleam/string.gleam)中找到更多关于字符串操作的实用示例。