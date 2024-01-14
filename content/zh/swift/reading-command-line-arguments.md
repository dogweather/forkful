---
title:                "Swift: 读取命令行参数"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 为什么

阅读命令行参数对于开发Swift应用程序时是一个非常有用的技巧。通过阅读命令行参数，您可以控制您的应用程序的行为，并根据用户的输入做出相应的响应。无论您是开发游戏、工具、还是其他类型的Swift应用程序，阅读命令行参数都会带来很多方便和便利。

## 如何

首先，您需要使用Swift的CommandLine类来读取命令行参数。然后，您可以使用该类的实例来访问传递给您的应用程序的所有命令行参数。让我们来看看一个简单的示例代码：

```Swift
import Foundation

let commandLine = CommandLine.arguments
// 命令行参数的第一个元素是应用程序的名称，所以我们从第二个元素开始遍历
for argument in commandLine.dropFirst() {
  print(argument)
}
```

如果您在命令行运行以上代码，并通过空格分隔多个参数传递给您的应用程序，例如`swift readCommandLineArguments.swift hello world`，您将会在控制台看到输出为：

```
hello 
world
```

您也可以使用CommandLine类来检查特定的命令行参数是否被传递。例如，如果您想要检查用户是否传递了`--verbose`参数，您可以使用`commandLine.contains("--verbose")`来检查。这在开发需要不同的调试模式的应用程序时非常有用。

## 深入挖掘

在阅读命令行参数时，需要注意以下几点：

- 您可以使用`commandLine.dropFirst()`来避免遍历应用程序名称。
- 根据需要，您可以将命令行参数转换为其他类型，例如字符串转换为整数或布尔值。
- 如果您需要使用命令行参数作为文件路径或其他类似的输入，建议使用`URL(fileURLWithPath:argument)`来转换参数为正确的格式。

通过深入挖掘CommandLine类的文档，您可以发现更多有用的方法来处理命令行参数。掌握这项技能将为您的Swift应用程序开发带来更多灵活性。

## 参考文献

- [Swift官方文档 - CommandLine](https://developer.apple.com/documentation/foundation/commandline)
- [Swift编程指南 - 输入输出](https://www.cnswift.org/optionals)
- [简单教程：使用Swift读取命令行参数](https://www.hackingwithswift.com/articles/110/how-to-read-command-line-arguments-using-swig)

## 参见

- [Swift命令行工具开发教程](https://www.raywenderlich.com/1568956-swift-command-line-tool-tutorial-for-beginners)
- [用Swift构建命令行应用程序](https://www.avanderlee.com/swift/command-line-tool/)
- [如何用Swift编写一个简单的命令行应用程序](https://zonneveld.dev/build-a-command-line-application-in-swift/)