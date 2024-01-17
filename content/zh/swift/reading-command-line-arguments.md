---
title:                "阅读命令行参数"
html_title:           "Swift: 阅读命令行参数"
simple_title:         "阅读命令行参数"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 做什么和为什么？

读取命令行参数是指在命令行输入程序时，程序能够接收并处理输入的数据。程序员通常会这样做是因为它使得程序更加灵活，能够接受用户的输入，并根据输入做出相应的操作。

## 如何：

```Swift
// 举例：读取命令行参数
import Foundation

let arguments = CommandLine.arguments

// 打印接收到的所有参数
// 例如：命令行输入：Swift program.swift "Hello, World!"
// 输出：["Swift","program.swift","Hello, World!"]

print(arguments)
```

```Swift
// 举例：根据命令行参数做出不同操作
import Foundation

let arguments = CommandLine.arguments

// 判断第一个参数是否为“generate”
if arguments[0] == "generate" {
    // 如果是，则打印第二个参数
    print("生成了一个新的文件：\(arguments[1])")
} else {
    // 如果不是，则打印错误信息
    print("无效的命令")
}
```

## 深入了解：

读取命令行参数在程序设计中的历史悠久，使用范围广泛。除了使用CommandLine外，还有一些库可以用来处理命令行输入，如Swift Argument Parser。在实现上，读取命令行参数需要使用CommandLine对象和数组来获取用户的输入，并对其进行相应的处理。

## 参考链接：

- [Swift Argument Parser](https://github.com/apple/swift-argument-parser)
- [CommandLine](https://developer.apple.com/documentation/foundation/commandline)