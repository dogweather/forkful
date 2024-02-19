---
aliases:
- /zh/swift/writing-a-text-file/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:32.300544-07:00
description: "\u5728 Swift \u4E2D\u5199\u5165\u6587\u672C\u6587\u4EF6\u5141\u8BB8\u4F60\
  \u6301\u4E45\u6027\u5730\u5C06\u5B57\u7B26\u4E32\u6570\u636E\u5B58\u50A8\u5728\u6587\
  \u4EF6\u7CFB\u7EDF\u4E0A\uFF0C\u8FD9\u5BF9\u4E8E\u4FDD\u5B58\u914D\u7F6E\u8BBE\u7F6E\
  \u3001\u7528\u6237\u6570\u636E\u6216\u65E5\u5FD7\u7B49\u4EFB\u52A1\u81F3\u5173\u91CD\
  \u8981\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u5728\
  \u5E94\u7528\u542F\u52A8\u4E4B\u95F4\u4FDD\u6301\u6570\u636E\u3001\u5728\u5E94\u7528\
  \u7684\u4E0D\u540C\u90E8\u5206\u4E4B\u95F4\u5171\u4EAB\u6570\u636E\uFF0C\u6216\u5BFC\
  \u51FA\u6570\u636E\u4F9B\u5176\u4ED6\u7A0B\u5E8F\u4F7F\u7528\u3002"
lastmod: 2024-02-18 23:08:59.458251
model: gpt-4-0125-preview
summary: "\u5728 Swift \u4E2D\u5199\u5165\u6587\u672C\u6587\u4EF6\u5141\u8BB8\u4F60\
  \u6301\u4E45\u6027\u5730\u5C06\u5B57\u7B26\u4E32\u6570\u636E\u5B58\u50A8\u5728\u6587\
  \u4EF6\u7CFB\u7EDF\u4E0A\uFF0C\u8FD9\u5BF9\u4E8E\u4FDD\u5B58\u914D\u7F6E\u8BBE\u7F6E\
  \u3001\u7528\u6237\u6570\u636E\u6216\u65E5\u5FD7\u7B49\u4EFB\u52A1\u81F3\u5173\u91CD\
  \u8981\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u5728\
  \u5E94\u7528\u542F\u52A8\u4E4B\u95F4\u4FDD\u6301\u6570\u636E\u3001\u5728\u5E94\u7528\
  \u7684\u4E0D\u540C\u90E8\u5206\u4E4B\u95F4\u5171\u4EAB\u6570\u636E\uFF0C\u6216\u5BFC\
  \u51FA\u6570\u636E\u4F9B\u5176\u4ED6\u7A0B\u5E8F\u4F7F\u7528\u3002"
title: "\u7F16\u5199\u6587\u672C\u6587\u4EF6"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 Swift 中写入文本文件允许你持久性地将字符串数据存储在文件系统上，这对于保存配置设置、用户数据或日志等任务至关重要。程序员经常这么做是为了在应用启动之间保持数据、在应用的不同部分之间共享数据，或导出数据供其他程序使用。

## 如何操作：

### 使用 Swift 标准库

Swift 的标准库包含了写入文本文件所需的所有工具。这是一个基本方法：

```swift
import Foundation

let content = "Hello, Wired readers! Learning Swift is fun."
let filePath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] as String
let fileName = "\(filePath)/example.txt"

do {
    try content.write(toFile: fileName, atomically: false, encoding: String.Encoding.utf8)
    print("文件成功写入")
} catch let error as NSError {
    print("写入 URL 失败：\(fileName), 错误： " + error.localizedDescription)
}
```

这段代码将一个字符串写入名为 `example.txt` 的文件中，位于文档目录。它使用 Swift 的 do-try-catch 错误处理来处理潜在的错误。

### 使用 FileManager 获得更多控制

为了更多地控制文件属性或检查文件是否已经存在，可以使用 `FileManager`：

```swift
import Foundation

let fileManager = FileManager.default
let directories = fileManager.urls(for: .documentDirectory, in: .userDomainMask)
if let documentDirectory = directories.first {
    let fileURL = documentDirectory.appendingPathComponent("example.txt")
    let content = "探索 Swift 进行文件管理是令人启迪的。"

    if fileManager.fileExists(atPath: fileURL.path) {
        print("文件已存在")
    } else {
        do {
            try content.write(to: fileURL, atomically: true, encoding: .utf8)
            print("文件创建并成功写入")
        } catch {
            print("写入文件错误：\(error)")
        }
    }
}
```

### 使用第三方库

Swift 中一个用于文件系统操作的流行第三方库是 John Sundell 的 `Files`：

首先，通常通过 Swift 包管理器将 Files 添加到你的项目中。

```swift
// swift-tools-version:5.3
import PackageDescription

let package = Package(
    name: "YourPackageName",
    dependencies: [
        .package(url: "https://github.com/JohnSundell/Files", from: "4.0.0"),
    ],
    targets: [
        .target(
            name: "YourTargetName",
            dependencies: ["Files"]),
    ]
)
```

然后，使用它来写入文件：

```swift
import Files

do {
    let file = try File(path: "/path/to/your/directory/example.txt")
    try file.write(string: "Swift 和 Files 库结合使用非常强大。")
    print("使用 Files 库成功写入文件。")
} catch {
    print("发生错误：\(error)")
}
```

有了 `Files` 库，文件处理变得更加直接，允许你专注于应用程序的业务逻辑，而不是文件管理的繁琐细节。
