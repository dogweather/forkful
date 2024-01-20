---
title:                "连接字符串"
html_title:           "C: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## 1. 它是什么，为什么？

字符串拼接是将两个或多个字符串连接在一起的过程。程序员那么做是因为我们需要创建或修改字符串，来输出更加复杂的语句。

## 2. 如何操作：

在 Kotlin 中，你有多种方式来拼接字符串。这里有几个例子：

1. 使用 `+` 运算符

```Kotlin
val str1 = "Hello, "
val str2 = "World!"
val str3 = str1 + str2
println(str3)  // 输出：“Hello, World!”
```

2. 使用 `plus()` 函数

```Kotlin
val str1 = "Ni"
val str2 = "hao"
val str3 = str1.plus(str2)
println(str3)  // 输出：“Nihao”
```

3. 使用字符串模板

```Kotlin
val name = "World"
val greeting = "Hello, $name!"
println(greeting)  // 输出：“Hello, World!”
```

## 3. 深入探索：

序列拼接颇具历史，C语言使用`strcat`函数而Java则提供了`StringBuilder`。Kotlin 提供的方法既简洁又高效，更适合现代编程。

替代方案包括使用 `StringBuilder` 或 `StringBuffer` 类，这两个类提供了 `append()` 方法，可以高效地处理大量的字符串拼接。

注意到，如果你在一次操作中拼接大量的字符串，使用 `+` 运算符或 `plus()` 函数可能会影响你的程序效率。这是因为创建新的字符串实例是一项开销较大的操作。

## 4. 参考资料：

请参阅 Kotlin 的官方文档，以了解更多关于字符串拼接的信息：

- [Kotlin官方字符串文档](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/)
- [Kotlin官方StringBuilder文档](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string-builder/)