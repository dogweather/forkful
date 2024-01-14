---
title:                "Kotlin: 打印调试输出"
programming_language: "Kotlin"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

为什么：有时候在编写程序时，我们需要在不破坏代码结构的情况下，观察程序执行的过程。这时，打印调试信息就十分有用。

如何：在Kotlin中，可以使用```print()```或```println()```函数来打印调试信息，这样可以将文本输出到控制台中。例如：

```Kotlin
var name = "John"
var age = 25

println("Name: $name")
println("Age: $age")
```

这个例子将会输出以下内容：

```
Name: John
Age: 25
```

深入了解：除了使用```print()```和```println()```函数外，还可以使用```Log```类来打印调试信息。这个类提供了更多的调试选项，例如设置打印级别、添加标签等。另外，还可以使用第三方库来提供更强大的调试功能，例如Android中的```Timber```库。

## 参考链接：

- Kotlin文档：https://kotlinlang.org/docs/basic-syntax.html#printing-things
- Android开发调试技巧：https://developer.android.com/studio/debug
- Timber库：https://github.com/JakeWharton/timber

## 参考文献：

无。