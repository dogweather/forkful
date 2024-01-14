---
title:                "Kotlin: 字符串转换为小写"
simple_title:         "字符串转换为小写"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么

字符串是编程中经常遇到的一种数据类型，而通常情况下，一个字符串不可能只有一个大小写形式。因此，将字符串转换为小写形式是非常有用的，可以让我们更轻松地处理和比较字符串。在Kotlin中，我们可以使用内置的函数来快速转换一个字符串为小写形式。

## 怎么做

首先，在你的Kotlin项目中创建一个字符串变量，比如我们这里用一个名为`str`的变量来做示例。然后，在变量后面加上句点`.lowercase()`，就可以将其转换为小写形式了。比如，如果我们有一个名为`str`的变量，它包含的是`"Hello WORLD!"`这样的字符串，那么我们使用`str.lowercase()`来转换后，得到的结果就是`"hello world!"`。下面是完整的代码示例：

```Kotlin
val str = "Hello WORLD!"
println(str.lowercase()) // Output: "hello world!"
```
值得注意的是，这个函数并不会改变原始的字符串变量，而是通过创建一个新的字符串返回转换后的结果。

## 深入了解

以上我们介绍了如何使用内置的`.lowercase()`函数来快速转换字符串为小写形式，但是有时候我们可能需要更具自定义的转换规则。对于这种情况，我们可以使用`.map()`函数来自定义如何转换字符串中的每一个字符。下面是一个例子：

```Kotlin
val str = "Hello WORLD!"
val customLowerCase = str.map {
    if (it.isUpperCase()) it.toLowerCase() else it
}
println(customLowerCase) // Output: "hello world!"
```

在这个例子中，我们使用了`.map()`函数来遍历字符串中的每一个字符，并使用`it`来表示当前遍历的字符。如果`it`是大写字母，我们就将其转换为小写形式，否则保持原样。最终，我们得到了和使用内置`.lowercase()`函数相同的结果。

## 参考链接

- [Kotlin官方文档：字符串转换](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-strings/index.html)
- [Kotlin中的字符串函数完全指南](https://www.programiz.com/kotlin-programming/strings)