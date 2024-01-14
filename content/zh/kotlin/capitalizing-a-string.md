---
title:                "Kotlin: 将字符串转换为大写"
simple_title:         "将字符串转换为大写"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么

当我们需要将字符串的首字母变成大写时，就需要用到capitalizing。这可以在展示数据或创建用户界面时非常有用。

## 如何做

首先，我们需要定义一个字符串变量：

```Kotlin
val text = "hello world"
```

然后，我们可以使用Kotlin中的```.capitalize()``` 方法来将字符串的首字母大写：

```Kotlin
val capitalizedText = text.capitalize()
```

当我们打印出```capitalizedText```时，输出为 ```Hello world```。如果我们想要将整个字符串都变成大写，可以使用```upperCase```方法：

```Kotlin
val upperCaseText = text.upperCase()
```

这将输出 ```HELLO WORLD```。另外，我们也可以使用 ```lowerCase``` 方法将字符串变成小写。

## 深入了解

在Kotlin中，我们可以通过创建一个自定义扩展函数来实现字符串capitalizing的方法。比如，我们可以创建一个新的函数```customCapitalize()```：

```Kotlin
fun String.customCapitalize() : String {
     return this[0].toUpperCase() + this.substring(1).toLowerCase()
}
```

然后，我们就可以对任意字符串进行capitalizing了：

```Kotlin
val customText = text.customCapitalize()
```

这个函数会首先将首字母变成大写，然后将剩余字母变成小写。这样，无论输入的字符串是什么，我们都可以得到正确的capitalized版本。

## 看看这些

- [Kotlin文档](https://kotlinlang.org/docs/reference/capitalization.html)
- [Kotlin String工具类](https://blog.mindorks.com/understanding-kotlin-string-utils)

## 参考链接

- [Why strings capitalization?](https://cmake.org/pipermail/cmake-developers/2002-October/000350.html)
- [How to capitalize a string in Kotlin?](https://stackoverflow.com/questions/43035202/how-to-capitalize-a-string-in-kotlin)