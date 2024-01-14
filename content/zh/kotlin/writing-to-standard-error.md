---
title:    "Kotlin: 将数据写入标准错误流"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

为什么：当我们在编写Kotlin代码时，有时候我们需要向标准错误输出（standard error）写入一些信息。这可以用来帮助我们调试代码，并且使得错误信息更容易被发现和解决。

# 为什么要向标准错误输出写入信息？

在软件开发过程中，我们经常会遇到一些错误，这些错误可能会导致程序运行失败或者产生不期望的结果。为了排查这些错误，我们需要能够快速定位到问题所在，并且及时解决，以保证程序的正常运行。而在Kotlin中，我们可以使用标准错误输出来输出错误信息，从而帮助我们更快地找到并修复问题。

# 如何向标准错误输出写入信息

要向标准错误输出写入信息，我们可以使用Kotlin标准库中的System类的方法eprintln()。下面是一个简单的例子，演示如何使用这个方法：

```Kotlin
fun main() {
    val name = "Jack"
    if (name == "Jack") {
        println("Hello, Jack!")
    } else {
        System.err.println("Name does not match!")
    }
}
```

输出结果：

```
Hello, Jack!
```

在这个例子中，当用户输入的姓名与“Jack”不匹配时，我们使用System类的eprintln()方法向标准错误输出打印一条错误信息。

# 深入了解标准错误输出

向标准错误输出写入信息与向标准输出（standard output）写入信息的方法类似。唯一的区别在于，标准输出用于打印正常的程序输出信息，而标准错误输出用于打印错误信息。另外，我们可以使用Kotlin标准库中的System类的out和err属性来直接访问标准输出和标准错误输出流。

如果需要重定向标准输出或标准错误输出，我们可以使用System类的setOut()和setErr()方法来指定要重定向的输出流。这在测试代码时非常有用，因为我们可以将输出重定向到一个文件中，以便后续查看。

# 参考链接

- Kotlin官方文档：https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-system/
- Kotlin标准库：https://kotlinlang.org/api/latest/jvm/stdlib/index.html
- Java标准库：https://docs.oracle.com/javase/8/docs/api/