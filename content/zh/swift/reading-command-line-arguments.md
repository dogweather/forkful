---
title:                "读取命令行参数"
html_title:           "C: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 什么和为什么?
读取命令行参数是从用户那里接收输入数据的一种方式，这些输入数据会以命令行参数的形式在程序启动时提供。编程者这样做是为了使程序更加通用和灵活，可以根据不同的输入调整其行为。

## 怎样做:
在Swift中，我们可以通过访问`CommandLine.arguments`数组来获取命令行参数。每个参数都是一个字符串，首个元素即是执行程序的路径。

```Swift
let arguments = CommandLine.arguments

print("总共有 \(arguments.count) 个命令行参数")
for argument in arguments {
    print("参数: \(argument)")
}
```
如果你将上述代码保存为`CommandLineArgs.swift`然后运行`swift CommandLineArgs.swift 参数1 参数2 参数3`，你将会看到：

```
总共有 4 个命令行参数
参数: CommandLineArgs.swift
参数: 参数1
参数: 参数2
参数: 参数3
```

## 深入挖掘
命令行参数在UNIX和Linux系统中具有深远的历史，被广泛应用于各种脚本和工具中。当然，除了使用`CommandLine.arguments`获取命令行参数外，你还可以使用`ProcessInfo.processInfo.arguments`，效果相同。

Swift实现读取命令行参数的机制其实很简单。在程序启动时，命令行参数被以字符串数组的形式传给程序。其中第一个元素是用于启动程序的命令，剩余的元素则是各个参数，按照命令行中的顺序排列。

## 另请参阅
2. Swift官方文档: [命令行参数](https://docs.swift.org/swift-book/LanguageGuide/Functions.html#ID158)