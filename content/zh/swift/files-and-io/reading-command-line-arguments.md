---
date: 2024-01-20 17:57:00.146107-07:00
description: "\u5728Swift\u4E2D\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570\u5141\u8BB8\
  \u4F60\u7684\u7A0B\u5E8F\u63A5\u53D7\u5916\u90E8\u8F93\u5165\u6570\u636E\u3002\u7A0B\
  \u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u7075\u6D3B\u5730\u63A7\u5236\u7A0B\
  \u5E8F\u884C\u4E3A\uFF0C\u800C\u65E0\u9700\u6539\u53D8\u4EE3\u7801\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:45.741543-07:00'
model: gpt-4-1106-preview
summary: "\u5728Swift\u4E2D\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570\u5141\u8BB8\u4F60\
  \u7684\u7A0B\u5E8F\u63A5\u53D7\u5916\u90E8\u8F93\u5165\u6570\u636E\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u7075\u6D3B\u5730\u63A7\u5236\u7A0B\u5E8F\
  \u884C\u4E3A\uFF0C\u800C\u65E0\u9700\u6539\u53D8\u4EE3\u7801\u3002"
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
在Swift中读取命令行参数允许你的程序接受外部输入数据。程序员这样做是为了灵活地控制程序行为，而无需改变代码。

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
