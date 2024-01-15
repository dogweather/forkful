---
title:                "读取命令行参数"
html_title:           "Swift: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 为什么

如果你在编写一个命令行工具或者脚本，读取命令行参数是一个非常重要的技能。通过它，你可以让用户在运行程序时通过命令行传入不同的参数，实现更灵活的操作和定制。

# 如何实现

读取命令行参数的方法很简单，只需要使用Swift中的CommandLine类。下面是一个例子：

```Swift
// 声明接收参数的变量
var inputStrings = CommandLine.arguments
// 移除默认的第一个参数，也就是程序的路径
inputStrings.removeFirst()

// 遍历并输出所有传入的参数
for inputString in inputStrings {
    print(inputString)
}
```

假如你把上面的代码保存在一个叫"CommandLineArgs.swift"的文件中，然后在命令行运行它，你会得到类似下面的输出：

```
$ swift CommandLineArgs.swift hello world
hello
world
```

# 深入了解

在这里，我们使用的CommandLine类是一个类型安全的命令行参数读取器。它提供了一些方便的方法来解析和存储用户传入的参数。比如，你可以使用`addArgument`方法来添加一个命令行参数，使用`arguments`属性来获取所有参数的数组。除此之外，你也可以通过设置`CommandLine.arguments`来模拟命令行传入的参数，便于调试和测试。

# 参考链接

- [Swift中的CommandLine文档](https://developer.apple.com/documentation/foundation/command_line)
- [如何使用Swift编写命令行工具](https://medium.com/@vxmute/building-command-line-tools-with-swift-3-dab914632c8a)
- [Swift命令行参数解析工具: SwiftCLI](https://github.com/jakeheis/SwiftCLI)
- [Swift命令行开发的一个实例: Swiftline](https://github.com/Swiftline/Swiftline)