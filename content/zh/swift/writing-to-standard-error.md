---
title:                "写入标准错误"
html_title:           "Arduino: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
标准错误（stderr）是输出错误信息的通道，与标准输出（stdout）不同。编程时，这样做能让我们区分正常输出与错误信息，便于调试和日志记录。

## 如何做：
```Swift
import Foundation

// 向标准错误输出消息
func eprint(_ message: String) {
    guard let data = "\(message)\n".data(using: .utf8) else { return }
    FileHandle.standardError.write(data)
}

// 使用
eprint("发生了一个错误！")
```
样本输出：
```
发生了一个错误！
```
请注意，这种错误不会出现在普通的输出流中。

## 深入探索：
在早期的Unix系统中，标准错误就已存在，其目的是将错误信息与正常输出分开。Swift语言直接继承了这一概念。尽管`print()`可以处理大多数输出任务，但它只写入到标准输出。如果你使用Linux或macOS终端，你可以利用重定向操作将标准输出与标准错误分开，例如：`yourprogram > output.txt 2> error.log`。Swift中写入标准错误需手动获取`FileHandle`，然后使用`write(_:)`方法。

## 参考链接：
- Swift 官方文档：[https://docs.swift.org/swift-book/](https://docs.swift.org/swift-book/)
- Unix Philosophy：[https://en.wikipedia.org/wiki/Unix_philosophy](https://en.wikipedia.org/wiki/Unix_philosophy)