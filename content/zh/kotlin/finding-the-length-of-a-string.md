---
title:    "Kotlin: 查找字符串的长度"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 为什么
在编程中，我们经常需要处理字符串（string），即一系列的字符。而对于字符串的操作，其中一个基本的任务就是要找出字符串的长度。字符串的长度指的是字符串中字符的个数，这在很多情况下都是非常有用的。因此掌握如何找到字符串的长度也是编程中必备的技能之一。在这篇文章中，我们将学习使用 Kotlin 来找到字符串的长度，让我们一起来看看吧！

# 如何
为了找出一个字符串的长度，我们可以使用 Kotlin 中的 `.length` 方法。下面是一个示例代码：

```Kotlin
val str = "Hello, World!"
val length = str.length
println(length) // 输出 13
```

首先，我们创建了一个名为 `str` 的字符串变量，它包含了 `Hello, World!` 这个字符串。然后，我们使用 `.length` 方法来获取字符串的长度，并把结果存储在 `length` 变量中。最后，我们使用 `println()` 方法来打印 `length` 变量的值，即字符串的长度。在这个例子中，字符串的长度为 13 个字符。

除了使用 `.length` 方法之外，我们也可以使用 `count()` 方法来找到字符串的长度。下面是另一个示例代码：

```Kotlin
val str = "This is a string"
val length = str.count()
println(length) // 输出 16
```

同样地，我们也首先创建了一个包含字符串的变量 `str`。然后，我们使用 `.count()` 方法来计算字符串的字符数，并把结果存储在 `length` 变量中。最后，我们打印出了 `length` 变量的值，即字符串的长度。在这个例子中，字符串的长度为 16 个字符。

需要注意的是，无论是使用 `.length` 方法还是 `count()` 方法，它们都是返回一个 `Int` 类型的值，即整数类型。因此，我们可以把它们存储在一个变量中，或者直接在打印语句中使用。

# 深入探讨
现在，我们已经知道了如何使用 Kotlin 来找到字符串的长度，但是在背后到底发生了什么呢？为了更好地理解这个过程，让我们来深入探讨一下。

在 Kotlin 中，字符串是一个 `String` 类型的对象。而 `.length` 方法和 `count()` 方法都属于 `String` 类型的扩展方法。也就是说，它们是专门为 `String` 类型准备的方法。这两个方法内部实现的逻辑是一样的，它们都是通过遍历字符串中的每个字符，并计数来得到字符串的长度。

另外，`.length` 方法和 `count()` 方法都有一个重载（overload）的版本，可以接受一个 lambda 表达式作为参数。通过这种方式，我们可以对字符串中的每个字符进行操作，例如判断某个字符是否符合我们的要求。这样，我们就可以在字符串的基础上进行更复杂的操作。

# 参考链接
- [Kotlin - Working with Strings](https://kotlinlang.org/docs/basic-syntax.html#working-with-strings)
- [Kotlin - String Extensions](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/index.html)
- [Kotlin - String count() method](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/with.html#count)
- [Kotlin - String length property](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/length.html)

# 另请参阅
- [Kotlin - 字符串的基本操作](https://www.jianshu.com/p/820845dd3665)
- [Kotlin - 导入包和扩展方法](https://www.jianshu.com/p/bc3b1774a844)
- [Kotlin