---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:38.461764-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Swift \u7684 Foundation \u6846\u67B6\u63D0\
  \u4F9B\u4E86 `FileManager` \u7C7B\uFF0C\u5B83\u6709\u7BA1\u7406\u6587\u4EF6\u7CFB\
  \u7EDF\u7684\u65B9\u6CD5\u3002\u60A8\u53EF\u4EE5\u4F7F\u7528 `FileManager` \u6765\
  \u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\u3002\u8FD9\u91CC\u662F\u5982\u4F55\
  \u505A\u5230\u8FD9\u4E00\u70B9\u7684\u4EE3\u7801\u7247\u6BB5\uFF1A."
lastmod: '2024-04-05T22:38:47.323436-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A Swift \u7684 Foundation \u6846\u67B6\u63D0\
  \u4F9B\u4E86 `FileManager` \u7C7B\uFF0C\u5B83\u6709\u7BA1\u7406\u6587\u4EF6\u7CFB\
  \u7EDF\u7684\u65B9\u6CD5\u3002\u60A8\u53EF\u4EE5\u4F7F\u7528 `FileManager` \u6765\
  \u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\u3002\u8FD9\u91CC\u662F\u5982\u4F55\
  \u505A\u5230\u8FD9\u4E00\u70B9\u7684\u4EE3\u7801\u7247\u6BB5\uFF1A."
title: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728"
weight: 20
---

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
