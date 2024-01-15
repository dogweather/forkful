---
title:                "连接字符串"
html_title:           "Kotlin: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

# 为什么

字符串拼接是一种常见的编程技术，它可以在不同的文本之间添加一定的间距或者连接起来形成一个长的字符串。这在创建文本消息、生成报告或者处理文件路径时都非常有用。

# 如何做

要在Kotlin中拼接字符串，我们可以使用 "+" 符号来连接两个字符串。例如，如果我们想要创建一个包含"Hello"和"World"的字符串，我们可以简单地使用：

```Kotlin
val message = "Hello" + " " + "World"
println(message) // 输出：Hello World
```

除此之外，我们也可以使用字符串模板来拼接字符串。字符串模板是一种特殊的字符串，它可以包含类似变量和表达式的占位符。当字符串模板被使用时，这些占位符将会被对应的变量或者表达式的值所替换。让我们来看一个示例：

```Kotlin
val name = "John"
val message = "Hello, $name!"
println(message) // 输出：Hello, John!
```

在上面的例子中，我们使用了字符串模板来将变量值动态插入到字符串中。除此之外，我们也可以使用花括号来包裹复杂的表达式，如下所示：

```Kotlin
val num1 = 10
val num2 = 5
val message = "The sum of $num1 and $num2 is ${num1 + num2}"
println(message) // 输出：The sum of 10 and 5 is 15
```

# 深入

在Kotlin中，字符串是不可变的，也就是说一旦字符串被创建，它的内容就不能被修改。所以，每次拼接字符串时，实际上都会创建一个新的字符串对象，这会对性能产生影响。为了避免这种情况，建议使用 `StringBuilder` 类，它允许我们对字符串进行修改而不需要创建新的对象。让我们来看一个示例：

```Kotlin
val message = StringBuilder()
message.append("Hello")
message.append(" ")
message.append("World")
println(message.toString()) // 输出：Hello World
```

在上面的例子中，我们使用 `append()` 方法来逐步添加内容到 `StringBuilder` 对象中。当我们需要输出最终的字符串时，我们只需要通过 `toString()` 方法来将其转换为普通的字符串。

# 查看也可以

- [Kotlin字符串拼接文档](https://kotlinlang.org/docs/reference/basic-types.html#string-literals)
- [使用字符串模板在Kotlin中拼接字符串](https://www.geeksforgeeks.org/kotlin-string-concatenation-and-string-templates/)