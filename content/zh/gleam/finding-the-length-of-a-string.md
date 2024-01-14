---
title:    "Gleam: 寻找字符串的长度"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

为什么：在编程中，我们经常需要使用字符串来存储和处理文本数据。了解如何找到字符串的长度是非常重要的，因为它可以帮助我们更有效地操作数据。

怎么做：下面是在Gleam中找到字符串长度的代码示例和输出结果，你可以通过运行这些代码来学习如何实现这个功能。

```Gleam
fn main() {
  let string = "Hello World";
  let length = string.length();

  io.print("字符串长度为：");
  io.print(length);
}

// 输出结果：
// 字符串长度为： 11
```

深入分析：在Gleam中，我们可以使用`string.length()`函数来找到字符串的长度。这个函数会返回一个整数值，表示字符串中字符的个数。需要注意的是，中文字符在Gleam中算作一个字符长度。

另外，我们也可以通过遍历字符串的每一个字符来手动获取长度。不过，使用`string.length()`函数更加简单和高效。

See Also（相关链接）：
- Gleam官方文档：https://gleam.run/
- 字符串处理相关方法：https://gleam.run/book/core-modules.html#string