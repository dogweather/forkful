---
title:                "Kotlin: 提取子字符串"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么要提取子字符串？

在编程中，我们经常需要从一个较长的字符串中提取出我们想要的部分。这就是提取子字符串的作用。通过提取子字符串，我们可以更方便地处理数据和字符串，使得代码更加简洁高效。

## 如何做到提取子字符串

在Kotlin中，提取子字符串有多种方法。对于普通字符串，我们可以使用 `substring()` 方法来提取指定范围内的子字符串。例如，我们有一个字符串 `val str = "Hello World"`，我们想要提取出 `World` 部分，可以这样写：

```Kotlin
val str = "Hello World"
val substring = str.substring(6)
print(substring) // Output: World
```

除了`substring()`方法，我们还可以使用 `slice()` 方法来提取指定的字符索引位置。例如，我们想要提取 `World` 这个单词，可以这样写：

```Kotlin
val str = "Hello World"
val substring = str.slice(6..10)
print(substring) // Output: World
```

当然，我们也可以使用正则表达式来提取符合特定规则的子字符串。例如，我们想要从一串不规则的文本中提取出所有的邮箱地址，可以这样写：

```Kotlin
val emailRegex = Regex("[a-zA-Z0-9]+@[a-zA-Z0-9]+\\.[a-z]{2,}")
val str = "Contact me at john.doe@example.com or jane.doe@example.com"
val emails = emailRegex.findAll(str).map { it.value }.toList()
print(emails) // Output: [john.doe@example, jane.doe@example.com]
```

## 深入了解提取子字符串

除了上述提到的方法，Kotlin还提供了其他一些提取子字符串的方法，比如 `drop()`、`take()`、`substringBefore()`、`substringAfter()`等。每种方法都有不同的用途，在编写代码时可以灵活运用。

此外，Kotlin中还有一个很方便的内置类 `StringTokenizer`，可以帮助我们更快速地提取子字符串。通过指定分隔符，我们可以轻松地将一个长字符串拆分成多个子字符串。例如，我们想要提取出 `Hello` 和 `World` 这两个单词，可以这样写：

```Kotlin
val str = "Hello World"
val tokens = StringTokenizer(str, " ")
print(tokens.nextToken()) // Output: Hello
print(tokens.nextToken()) // Output: World
```

## 参考链接

- [Kotlin字符串处理](https://www.kotlincn.net/docs/reference/strings.html)
- [Kotlin字符串操作](https://www.tutorialspoint.com/kotlin/kotlin_strings.htm)
- [Kotlin入门系列之字符串处理](https://zhuanlan.zhihu.com/p/34373278)

## 相关阅读 (See Also)

- [Kotlin入门系列之提取子字符串](https://zhuanlan.zhihu.com/p/34313831)
- [Kotlin文本处理实用工具库](https://blog.csdn.net/u013140070/article/details/77814926)
- [Kotlin正则表达式基础教程](https://www.jianshu.com/p/b0663a7a0a65)