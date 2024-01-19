---
title:                "将字符串转换为小写"
html_title:           "Arduino: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 是什么与为什么?

字符串转换为小写就是将字符串中的所有大写字母更改为对应的小写字母。这项操作对于创建不区分大小写的字符串匹配操作或者用户输入标准化非常有用。

## 如何操作:

使用Kotlin的 `toLowerCase()` 函数，可以将字符串转换为小写。下面是一些代码示例和结果:

```Kotlin
fun main() {
    val str = "HELLO WORLD"
    println(str.toLowerCase())
}
```

运行这段代码，输出结果为:

```
hello world
```

## 深入探究

在实现细节上，`toLowerCase()`转换方法在大多数情况下会呈现预期的行为，但在处理一些特别的语言环境时可能需要额外的注意。 

例如：

```Kotlin
fun main() {
    val str = "HELLO WORLD İ"
    println(str.toLowerCase())
}
```

在默认设置下，大写的 "İ"会被转换为 "i"，而不是 "ı"。这可能会有问题，尤其是在一些特别的语言环境比如土耳其语中。

你可以传递一个 `Locale` 实例到 `toLowerCase()` 函数来指定转换规则：

```Kotlin
fun main() {
    val str = "HELLO WORLD İ"
    println(str.toLowerCase(Locale.forLanguageTag("tr-TR")))
}
```

这段代码会正确的输出 "hello world ı"。

至于替代方案，你也可以使用 `String.fold()` 方法结合 `Char.toLowerCase()` 方法手动实现这个功能。

```Kotlin
fun main() {
    val str = "HELLO WORLD"
    println(str.fold("") { acc, c -> acc + c.toLowerCase() })
}
```

这段代码也会输出 "hello world"。

## 探究更多

- Kotlin官方文档: [toLowerCase()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html), [Char.toLowerCase()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html)
- [Kotlin `fold` function](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/fold.html) 
- [Locale-specific behavior](https://docs.oracle.com/javase/tutorial/i18n/locale/index.html)