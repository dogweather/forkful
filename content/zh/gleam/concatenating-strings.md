---
title:                "Gleam: 连接字符串"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

为什么：字符串拼接是编程中非常常用的操作，它可以帮助我们将多个字符串组合成一个完整的字符串，从而方便我们处理和使用信息。

如何做：下面是一个使用Gleam语言拼接字符串的例子：

```Gleam
fn main() {
  let str1 = "Hello";
  let str2 = "World";
  let combined = str1 <> " " <> str2;
  IO.println(combined);
}
```

输出：Hello World

深入探讨：字符串拼接并不是仅仅将两个字符串简单地连接起来，它还涉及到一些复杂的操作。比如在Gleam中，我们可以使用<>符号来连接字符串，但是它也支持将多个字符串拼接成一个列表，然后通过join函数来组合成一个字符串。拼接字符串还可以用于构建动态的SQL语句，处理用户输入等。总之，在编程中灵活使用字符串拼接可以为我们带来很多方便和效率。

另请参阅：

- [Gleam教程](https://gleam.run/tutorials/getting-started)
- [Gleam官方文档](https://gleam.run)
- [了解更多关于字符串拼接的知识](https://www.w3schools.com/js/js_string_methods.asp)

查看也：

相关链接：

- [Gleam语言官网](https://gleam.run)
- [在GitHub上查看Gleam的源代码](https://github.com/gleam-lang/gleam)