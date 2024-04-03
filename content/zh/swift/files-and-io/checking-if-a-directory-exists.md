---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:38.461764-07:00
description: "\u5728\u6587\u4EF6\u7CFB\u7EDF\u4E2D\u68C0\u67E5\u76EE\u5F55\u662F\u5426\
  \u5B58\u5728\u5BF9\u4E8E\u4ECE Swift \u5E94\u7528\u7A0B\u5E8F\u5185\u90E8\u7BA1\u7406\
  \u6587\u4EF6\u7ED3\u6784\u81F3\u5173\u91CD\u8981\u3002\u8FD9\u4E00\u4EFB\u52A1\u4F7F\
  \u5F97\u5F00\u53D1\u8005\u80FD\u591F\u5728\u5C1D\u8BD5\u8BFB\u53D6\u6216\u5199\u5165\
  \u5B83\u4EEC\u4E4B\u524D\uFF0C\u9A8C\u8BC1\u76EE\u5F55\u7684\u5B58\u5728\u6027\uFF0C\
  \u4ECE\u800C\u907F\u514D\u53EF\u80FD\u7684\u8FD0\u884C\u65F6\u9519\u8BEF\u3002"
lastmod: '2024-03-13T22:44:48.174084-06:00'
model: gpt-4-0125-preview
summary: "\u5728\u6587\u4EF6\u7CFB\u7EDF\u4E2D\u68C0\u67E5\u76EE\u5F55\u662F\u5426\
  \u5B58\u5728\u5BF9\u4E8E\u4ECE Swift \u5E94\u7528\u7A0B\u5E8F\u5185\u90E8\u7BA1\u7406\
  \u6587\u4EF6\u7ED3\u6784\u81F3\u5173\u91CD\u8981\u3002\u8FD9\u4E00\u4EFB\u52A1\u4F7F\
  \u5F97\u5F00\u53D1\u8005\u80FD\u591F\u5728\u5C1D\u8BD5\u8BFB\u53D6\u6216\u5199\u5165\
  \u5B83\u4EEC\u4E4B\u524D\uFF0C\u9A8C\u8BC1\u76EE\u5F55\u7684\u5B58\u5728\u6027\uFF0C\
  \u4ECE\u800C\u907F\u514D\u53EF\u80FD\u7684\u8FD0\u884C\u65F6\u9519\u8BEF\u3002."
title: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728"
weight: 20
---

## 什么 & 为什么？
在文件系统中检查目录是否存在对于从 Swift 应用程序内部管理文件结构至关重要。这一任务使得开发者能够在尝试读取或写入它们之前，验证目录的存在性，从而避免可能的运行时错误。

## 如何操作：

Swift 的 Foundation 框架提供了 `FileManager` 类，它有管理文件系统的方法。您可以使用 `FileManager` 来检查目录是否存在。这里是如何做到这一点的代码片段：

```swift
import Foundation

let fileManager = FileManager.default
let path = "/path/to/your/directory"

if fileManager.fileExists(atPath: path, isDirectory: nil) {
    print("目录存在")
} else {
    print("目录不存在")
}
```

然而，这会同时检查文件和目录。如果您特别想要验证目录存在，您需要在 `isDirectory` 中传递一个指向布尔值的指针：

```swift
import Foundation

let fileManager = FileManager.default
let path = "/path/to/your/directory"
var isDirectory: ObjCBool = false

if fileManager.fileExists(atPath: path, isDirectory: &isDirectory), isDirectory.boolValue {
    print("目录存在")
} else {
    print("目录不存在")
}
```

### 使用第三方库

到目前为止，由于 `FileManager` 类的强大，通常不需要使用第三方库来检查 Swift 中的目录是否存在。然而，对于更复杂的文件操作和检查，像 John Sundell 的 **Files** 这样的库提供了一个更符合 Swift 风格的 API。

以下是你可能使用它的方式：

首先，通过 Swift 包管理器将 Files 添加到你的项目中。

然后，您可以这样检查目录是否存在：

```swift
import Files

do {
    _ = try Folder(path: "/path/to/your/directory")
    print("目录存在")
} catch {
    print("目录不存在")
}
```

注意：由于第三方库可能会更改，始终参考最新的文档以了解使用和最佳实践。
