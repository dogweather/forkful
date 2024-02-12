---
title:                "检查目录是否存在"
aliases:
- /zh/swift/checking-if-a-directory-exists/
date:                  2024-02-03T19:08:38.461764-07:00
model:                 gpt-4-0125-preview
simple_title:         "检查目录是否存在"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
