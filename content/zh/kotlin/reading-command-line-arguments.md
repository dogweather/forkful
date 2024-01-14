---
title:    "Kotlin: 读取命令行参数."
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 为什么

命令行参数是编写任何程序时非常重要的一部分。它们为我们提供了一种与程序交互的方式，可以让程序根据输入的不同参数执行不同的操作。读取命令行参数也可以帮助我们更容易地调试程序，发现错误并进行修改。因此，深入了解如何读取命令行参数是非常有价值的。

# 如何做

在Kotlin中，我们可以使用`args`数组来读取命令行参数。此数组包含了在命令行输入的所有参数。让我们看一个简单的示例，代码如下：

```Kotlin
fun main(args: Array<String>) {
    // 打印第一个参数
    println(args[0])
}
```

假设我们在命令行中输入`kotlin example.kt argument1 argument2`，则该程序的输出将是`argument1`。我们可以根据需要在程序中使用这些参数，例如根据输入的参数执行不同的计算或打印不同的信息。

# 深入了解

除了基本的命令行参数读取之外，Kotlin还提供了更多功能来帮助我们处理命令行参数。例如，我们可以使用`getOrNull`函数来检查数组中指定位置的参数是否存在，并在该参数不存在时返回null。我们还可以使用`getOptionValue`函数来获取键值对类型的参数，例如`--name=John`。另外，Kotlin还支持使用命令行参数来指定程序的默认运行参数，这些内容可以在官方文档中找到。

# 参考链接

- 官方文档：https://kotlinlang.org/docs/command-line.html
- 学习Kotlin：https://kotlincn.netlify.app/docs/tutorials/command-line.html
- Kotlin命令行参数教程：https://www.geeksforgeeks.org/kotlin-command-line-arguments/