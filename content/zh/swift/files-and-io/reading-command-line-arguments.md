---
date: 2024-01-20 17:57:00.146107-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) Swift\u4E2D\uFF0C\u7528`CommandLine`\u76F4\
  \u63A5\u8BBF\u95EE\u547D\u4EE4\u884C\u53C2\u6570\u3002\u770B\u4E2A\u7B80\u5355\u4F8B\
  \u5B50\uFF1A."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.464038-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) Swift\u4E2D\uFF0C\u7528`CommandLine`\u76F4\
  \u63A5\u8BBF\u95EE\u547D\u4EE4\u884C\u53C2\u6570\u3002\u770B\u4E2A\u7B80\u5355\u4F8B\
  \u5B50\uFF1A."
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
weight: 23
---

## How to: (如何操作：)
Swift中，用`CommandLine`直接访问命令行参数。看个简单例子：

```Swift
// main.swift
for argument in CommandLine.arguments {
    print(argument)
}
```

假设程序名为`app`，运行：

```bash
$ swift run app one two three
```

输出：

```
/path/to/app
one
two
three
```

第一个参数总是程序路径。其余的是传递给程序的参数。

## Deep Dive (深入研究)
- 历史上，像C语言这样的早期语言通过`main`函数的参数来读取命令行信息。Swift提供了更现代化的方法。
- 除了`CommandLine`，也可使用`ProcessInfo`获得更多环境信息。
- 实现细节：`CommandLine.arguments`是一个字符串数组(String Array)，保存所有命令行输入。

例如，判断参数个数：

```Swift
if CommandLine.argc < 2 {
    print("No arguments provided.")
} else {
    // Handle arguments
}
```

## See Also (另见)
- Swift官方文档中的[CommandLine](https://developer.apple.com/documentation/swift/commandline)类
- [ProcessInfo](https://developer.apple.com/documentation/foundation/processinfo)类文档
- Unix命令行教程，了解命令行基础
