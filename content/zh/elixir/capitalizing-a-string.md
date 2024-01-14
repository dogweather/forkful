---
title:                "Elixir: 《将字符串首字母大写》"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Elixir程序中的字符串大写，为什么要这么做？

## 为什么

在Elixir编程中，字符串大写可以让字符串更易读，更容易与其他字符串进行比较和匹配。

## 如何实现

在Elixir中，我们可以使用`String.upcase/1`函数来大写字符串。例如：

```Elixir
String.upcase("hello") # 输出 "HELLO"
String.upcase("elixir") # 输出 "ELIXIR"
```

## 深入探讨

在Elixir中，字符串是不可变的，这意味着我们不能直接修改字符串的值。所以，`String.upcase/1`函数实际上是创建一个新的大写字符串，而不是修改原来的字符串。这也是为什么在Elixir中，大多数字符串操作都会返回一个新的字符串。

此外，值得注意的是，`String.upcase/1`函数只会将小写字母转换为大写字母，其他字符（如数字和标点符号）不会受到影响。

## 查看更多

- [Elixir字符串文档](https://hexdocs.pm/elixir/String.html)
- [如何在Elixir中操作字符串](https://medium.com/@zw963/handle-string-in-elixir-c8260dc33be8)
- [Elixir的字符串操作（视频）](https://www.youtube.com/watch?v=Kn1qniE79ZQ)

## 参考资料

[The Elixir Programming Language Website](https://elixir-lang.org/)