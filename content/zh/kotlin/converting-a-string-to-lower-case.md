---
title:                "将字符串转换为小写"
html_title:           "Kotlin: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么

有时候我们需要对字符串进行处理，比如检索、比较或者展示。而有些时候，我们希望字符串的大小写能够统一，便于处理。因此， Kotlin 提供了一个方便的方法来将字符串转换为小写字母，让我们来一起看看如何实现吧！

## 如何操作

在 Kotlin 中，使用 `toLowerCase()` 方法可以将一个字符串转换为小写。下面是一个简单的例子，展示了如何使用这个方法：

```Kotlin
val str = "KOTLIN PROGRAMMING"
val lowerCaseStr = str.toLowerCase()

println(lowerCaseStr)
```

输出结果为：

```
kotlin programming
```

## 深入了解

除了将字符串转换为小写外， Kotlin 还提供了其他几种方法来处理字符串的大小写。下面是一些常用的方法：

- `toUpperCase()`：将字符串转换为大写。
- `capitalize()`：将字符串的首字母大写。
- `decapitalize()`：将字符串的首字母小写。

除了这些方法外，我们还可以使用正则表达式来匹配并修改字符串中的大小写。例如，我们可以使用 `replace()` 方法来将字符串中的所有大写字母转换为小写字母：

```Kotlin
val str = "Kotlin Programming"
val replacedStr = str.replace(Regex("[A-Z]")) {
    it.value.toLowerCase()
}

println(replacedStr)
```

输出结果为：

```
kotlin programming
```

## 参考链接

- [Kotlin String - toLowerCase()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html)
- [Kotlin String - toUpperCase()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-upper-case.html)
- [Kotlin String - capitalize()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html)
- [Kotlin String - decapitalize()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/decapitalize.html)