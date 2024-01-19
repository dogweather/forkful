---
title:                "查找字符串的长度"
html_title:           "Javascript: 查找字符串的长度"
simple_title:         "查找字符串的长度"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 是什么？为什么？

找出字符串的长度，是确定一个字符串中字符的数量的过程。程序员这样做，能够更有效地管理和操作字符串。

## 具体怎么做：

在Gleam中获取字符串长度的代码示例：

```gleam
let str = "你好，世界"
let str_len = str.len()

print(str_len)  //输出结果为 5
```

## 深入了解

- 历史情况: Gleam在0.8.0版本开始支持`len`函数，用于获取字符串的长度。
- 替代方案: 在添加`len`函数之前，程序员需要通过遍历字符串的每一个字符以获取长度，效率相对较低。
- 实现细节: Gleam使用Rust的`str.len()`函数实现字符串长度的计算。它返回字符串的字节数，然而，对于UTF-8字符串，不是所有的字节都等同于字符。因此，Gleam的字符串长度操作在计算时，以Unicode标量值的数量为准。

## 还可以看看

- Gleam官方文档: [Gleam Strings](https://gleam.run/book/tour/strings.html)
- Rust官方文档: [str.len()](https://doc.rust-lang.org/std/primitive.str.html#method.len)