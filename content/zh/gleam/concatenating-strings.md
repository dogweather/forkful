---
title:                "连接字符串"
html_title:           "C: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## 是什么与为什么?
字符串连接就是要把两个或更多的字符串连接在一起。程序员之所以使用这种技术，是因为它使得代码编写更有效率，同时也灵活地创建和修改字符串。

## 如何操作:
以下是Gleam编程语言中的字符串连接示例:
在Gleam (v0.18.1) 中，我们使用 `++` 运算符进行字符串连接。

```Gleam
import gleam/string

fn main() {
  let str1 = "欢迎来到"
  let str2 = "Gleam世界"
  string.concat([str1, str2])
}
```
以上代码的输出是:
```Gleam
"欢迎来到Gleam世界"
```
## 深入探究:
1. 历史环境: 在编程语言的早期阶段，一般只支援通过链接字符来创建字符串。但随着编程语言的发展，现在大多数语言都支持直接的字符串连接操作。
2. 可选方案: 除了使用 `++` 运算符外，我们也可以使用 `string.append` 函数进行字符串连接。
3. 实现细节: 在Gleam中，`++` 运算符和 `string.concat` 函数遵循左侧关联性原则。

## 参考资料:
- Gleam官方文档对于字符串连接的详细说明: [Gleam 文档](https://gleam.run/book/tour/strings.html#consuming)
- 相关函数 `string.append` 的引用: [Gleam 文档](https://hexdocs.pm/gleam_stdlib/gleam/string.html#append)