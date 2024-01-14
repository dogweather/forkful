---
title:    "Swift: 读取命令行参数"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##为什么要读取命令行参数

读取命令行参数是一个很有用的技能，在Swift编程中经常会用到。它可以让你的程序在运行时接收用户输入的参数，从而实现更加灵活和个性化的功能。如果你想要了解如何在你的程序中读取命令行参数，继续阅读下去吧！

##如何读取命令行参数

首先，你需要使用CommandLine类来读取命令行参数。你可以通过将参数作为字符串数组传递给CommandLine对象来访问这些参数，例如：

```Swift
let arguments = CommandLine.arguments
```

接下来，你可以使用for循环来遍历这些参数，并对每一个参数进行操作。例如，假设你的程序需要接收两个整数作为参数，并将它们相加并打印出结果，你可以使用以下代码：

```Swift
for index in 1..<arguments.count {
    if let number = Int(arguments[index]) {
        let result = number + number
        print(result)
    }
}
```

当你运行程序时，你可以这样输入参数来测试这段代码：

```
命令行参数：3 5
```

输出结果将是：

```
6
10
```

##深入了解读取命令行参数

除了上面提到的基本用法，你还可以通过使用CommandLine对象提供的其他方法来进一步控制和处理命令行参数。例如，你可以通过给CommandLine对象传递一个字符串来检查是否存在某个特定的命令行参数：

```Swift
if CommandLine.arguments.contains("--debug") {
    //执行代码
}
```

你也可以通过给CommandLine对象传递两个字符串来设定默认值和用户输入值，从而实现命令行参数的可选输入功能：

```Swift
let argument = CommandLine.option("password", "-p")
```

当你在命令行中输入`-p`时，`argument`变量将被赋值为`password`。如果你没有输入`-p`，则`argument`变量将被赋值为nil。

##另请参阅

- [Swift编程语言指南](https://docs.swift.org/swift-book/)
- [Apple官方CommandLine文档](https://developer.apple.com/documentation/foundation/commandline)

感谢阅读本文，希望它能帮助你更好地理解和使用命令行参数。继续学习和探索Swift编程的世界，尽情享受编程的乐趣吧！