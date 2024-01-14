---
title:    "Kotlin: 将字符串转换为小写"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# 为什么要将字符串转换为小写
有时我们需要将字符串转换为小写，以便在处理文本时能够进行更精确的匹配。例如，当我们在搜索引擎中搜索关键词时，我们希望不区分大小写地找到相关的结果。

## 如何进行转换
我们可以使用Kotlin中的toLowerCase()函数来将字符串转换为小写。这个函数接受一个字符串作为参数，并返回一个新的字符串，其中所有的字符都被转换为小写。例如：

```Kotlin
val name = "Mandarin"
val lowerCaseName = name.toLowerCase()
println(lowerCaseName) // output: mandarin
```

## 深入了解字符串转换为小写
有时候我们可能需要处理特殊字符，比如多语言文本。在这种情况下，我们需要考虑字符的unicode值。对于每个字符，在转换为小写之前，我们可以使用unicode值来判断是否需要特殊处理。例如，处理带有重音符号的法语字符：

```Kotlin
val frenchName = "Élodie"
val lowerCaseFrenchName = frenchName.map {
    if (it.code == 201) {
        'e' // unicode值为201的字符为É，将其转换为e
    } else {
        it.toLowerCase() // 对于其他字符，使用toLowerCase()函数
    }
}.joinToString("")
println(lowerCaseFrenchName) // output: elodie
```

# 更多链接
- [Kotlin String类参考文档](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Kotlin字符串操作教程](https://www.baeldung.com/kotlin/string-operations)