---
title:                "Swift: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

为什么：读取命令行参数对于Swift编程人员非常重要，因为它可以让他们轻松地从命令行接收输入并根据不同的参数执行不同的操作。

如何：下面是一个简单的示例，展示如何读取命令行参数并根据参数输出不同的内容。

```Swift
let arguments = CommandLine.arguments

//检查是否有参数
if arguments.count > 1 {
    //使用第一个参数作为输入值
    let input = arguments[1]
    
    //根据输入值输出不同的内容
    switch input {
        case "1":
            print("这是第一个参数")
        case "2":
            print("这是第二个参数")
        default:
            print("未知参数")
    }
} else {
    print("请输入一个参数")
}
```

输出：

如果在命令行输入`swift run 1`，则输出为`这是第一个参数`；如果输入`swift run 2`，则输出为`这是第二个参数`。

深入探究：当在命令行输入参数时，Swift会将这些参数存储在`CommandLine`类的静态属性`arguments`中。使用`arguments`数组可以轻松地读取和操作命令行参数。除了使用`CommandLine`类，还可以使用`ProcessInfo`类来读取命令行参数。

参考链接：

- [Swift - Command Line Args Using CommandLine.arguments](https://www.tutorialspoint.com/swift_programming/swift_command_line_args.htm)
- [Command Line Applications](https://developer.apple.com/library/archive/documentation/CommandLine/Conceptual/CommandLine.pdf)

请参考：

参考链接（See Also）：

- [Swift Package Manager - Command Line Interface](https://swift.org/package-manager/#command-line-interface)
- [Swift Argument Parser library](https://github.com/apple/swift-argument-parser)