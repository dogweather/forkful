---
title:    "Kotlin: 提取子字符串"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

# 为什么要提取子字符串？

在编程中，经常会遇到需要从一个长的字符串中提取出部分内容的情况。这可以通过提取子字符串来实现。子字符串是指从原始字符串中截取出一部分字符组成的新字符串。这样的提取可以帮助我们更容易地处理和操作数据，提高代码的可读性和效率。

## 如何提取子字符串？

提取子字符串的步骤非常简单。首先，我们需要一个原始字符串和想要提取的起始位置和长度。在Kotlin中，可以使用`substring()`方法来提取子字符串。

```Kotlin
val str = "Hello, world!"
val substring = str.substring(7, 5)
println(substring) // 输出: world
``` 

`substring()`方法中，第一个参数是起始位置，第二个参数是需要提取的字符个数。上面的例子中，起始位置为7，表示从字符串中的第8个字符开始提取，提取的长度为5个字符，即`world`。

我们也可以将起始位置改为负数来从字符串的末尾开始计算，例如：

```Kotlin
val str = "Hello, world!"
val substring = str.substring(-6, 4)
println(substring) // 输出: worl
```

在这个例子中，起始位置为-6，表示从字符串的倒数第6个字符开始提取，提取的长度为4个字符，即`worl`。

如果我们只想提取字符串的一部分，例如从某个位置到结尾，可以省略第二个参数，例如：

```Kotlin
val str = "Hello, world!"
val substring = str.substring(7)
println(substring) // 输出: world!
```

省略第二个参数的意思是提取从起始位置到字符串结尾的所有字符。

## 深入了解提取子字符串

除了`substring()`方法，Kotlin还提供了其他几种提取子字符串的方法：

- `subSequence()`：与`substring()`类似，但返回一个`CharSequence`对象。
- `take()`：从字符串的开头提取指定数量的字符。
- `drop()`：从字符串的开头删除指定数量的字符。
- `slice()`：从字符串中按照指定的下标提取多个子字符串。

此外，还可以使用正则表达式来提取符合特定规则的子字符串。详细的内容可以参考Kotlin官方文档。

# See Also
- Kotlin官方文档：https://kotlinlang.org/docs/reference/basic-types.html#strings
- 利用正则表达式提取子字符串：https://www.baeldung.com/kotlin-regex
- 深入理解字符串操作：https://medium.com/@ognjen/time-saving-kotlin-string-operations-that-you-probably-dont-know-about-bc84a62ef3e1