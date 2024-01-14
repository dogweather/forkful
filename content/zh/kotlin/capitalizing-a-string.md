---
title:    "Kotlin: 将字符串首字母大写"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

为什么：字符串大写化的意义只需要 1-2 句话就可以解释清楚。

字符串大写化的作用是通过将字符串中每个字符的小写字母转换为大写字母，使得该字符串的所有字符都是大写。在某些情况下，我们可能需要将字符串转换为大写形式，例如在比较字符串时，不区分大小写可能会导致错误的结果。通过对字符串进行大写化，我们可以确保字符串的大小写统一，从而避免错误的发生。

## 如何实现

要实现字符串的大写化，我们可以使用 Kotlin 中的内置函数 `toUpperCase()`。这个函数接受一个字符串作为参数，并返回一个新的大写化后的字符串。下面是一个简单的示例代码，在 `main()` 函数中我们创建一个包含小写字母的字符串 `str`，然后使用 `toUpperCase()` 函数将其转换为大写形式，并将结果打印出来。

```Kotlin
fun main() {
    val str = "hello world"
    val strUpper = str.toUpperCase()
    println(strUpper)
}
```
输出结果为：
```
HELLO WORLD
```
除了使用 `toUpperCase()` 函数，我们也可以使用 Kotlin 的扩展函数 `capitalize()`。这个函数同样也接受一个字符串作为参数，并返回一个新的大写化后的字符串。不过与 `toUpperCase()` 不同的是，`capitalize()` 函数只会将字符串的第一个字符转换为大写，而不会改变其他字符的大小写。下面是一个示例代码，在 `main()` 函数中我们创建一个包含小写字母的字符串 `str`，然后使用 `capitalize()` 函数将其转换为大写形式，并将结果打印出来。

```Kotlin
fun main() {
    val str = "hello world"
    val strCapitalized = str.capitalize()
    println(strCapitalized)
}
```

输出结果为：
```
Hello world
```

## 深入了解

除了内置函数和扩展函数之外，我们也可以通过自己编写函数来实现字符串的大写化。通常，我们会使用循环来遍历字符串中的每个字符，并将其转换为大写形式。下面是一个示例代码，我们使用 `for` 循环遍历字符串中的每个字符，并使用 `toUpperCase()` 函数将其转换为大写形式。

```Kotlin
fun toUpperCase(str: String): String {
    var result = ""
    for (char in str) {
        result += char.toUpperCase()
    }
    return result
}

fun main() {
    val str = "hello world"
    val strUpper = toUpperCase(str)
    println(strUpper)
}
```

输出结果为：
```
HELLO WORLD
```

以上就是关于在 Kotlin 中实现字符串大写化的几种方法。通过选择不同的方法，我们可以根据自己的需求来实现字符串的大写化。

## 参考资料

- [Kotlin String.capitalize() function](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html)
- [Kotlin String.toUpperCase() function](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-upper-case.html)
- [How to convert a string to uppercase in Kotlin](https://www.techiedelight.com/convert-string-uppercase-kotlin/)