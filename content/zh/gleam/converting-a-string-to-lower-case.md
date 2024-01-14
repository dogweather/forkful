---
title:                "Gleam: 将字符串转换为小写"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么

为什么有时候我们需要把字符串转换为小写形式？这通常是因为我们需要在代码中比较字符串时忽略大小写。例如，当我们要验证一个用户输入的用户名或密码时，我们通常会把输入的字符串转换为小写形式，以避免用户因为大小写不同而无法成功登录。

## 如何做

```Gleam
let username = "JohnDoe"
let password = "p@$$w0rd"

// 把输入的用户名和密码都转换为小写形式
let lowercase_username = String.to_lower(username)
let lowercase_password = String.to_lower(password)

// 现在我们可以比较字符串，而不必担心大小写
if lowercase_username == "johndoe" && lowercase_password == "p@$$w0rd" {
    // 登录成功
    // do something
} else {
    // 登录失败
    // do something else
}
```

输出:

```
"johndoe"
"p@$$w0rd"
```

## 深入了解

字符串转换为小写形式的过程其实并不复杂。在计算机中，每个字符都有一个对应的 ASCII 码，其中大写字母和小写字母的 ASCII 码相差32个单位。因此，要把一个大写字母转换为小写字母，只需把它的 ASCII 码加上32。

在 Gleam 中，我们使用 `String.to_lower` 函数来把一个字符串转换为小写形式。这个函数会遍历字符串中的每一个字符，并根据它的 ASCII 码加或减32来转换大小写。如果字符本身不是大写字母，那么它的 ASCII 码不会受影响，仍然保持不变。

## 参考资料

Github: [Gleam语言官方文档](https://github.com/gleam-lang/gleam/tree/master/docs)

官网教程: [Gleam语言入门教程](https://gleam.run/book/introduction.html)

Stack Overflow: [如何在Gleam中转换字符串为小写？](https://stackoverflow.com/questions/50525664/how-to-convert-string-to-lower-case-in-erlang/50527619#50527619)

## 另请参阅

[Gleam语言：探索新兴的函数式编程语言](https://medium.com/@mandarinwriter/gleam%E8%AF%AD%E8%A8%80-%E6%8E%A2%E7%B4%A2%E6%96%B0%E5%85%B4%E7%9A%84%E5%87%BD%E6%95%B0%E5%BC%8F%E7%BC%96%E7%A8%8B%E8%AF%AD%E8%A8%80-32d685526f7a)